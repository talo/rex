use std::{env, io, path::PathBuf};

use anyhow::Context as _;
use clap::Parser as CmdLnArgsParser;
use rex_ast::types::Type;
use rex_engine::{error::sprint_trace_with_ident, ftable::Ftable, Context};
use rex_lexer::Token;
use rex_parser::Parser;
use tokio::{fs::File, io::AsyncReadExt};
use tracing_subscriber::{fmt, layer::SubscriberExt as _, util::SubscriberInitExt as _, EnvFilter};

/// The Rex interpreter.
#[derive(CmdLnArgsParser, Debug)]
#[clap(author, version, about, long_about = None)]
struct CmdLnArgs {
    #[command(subcommand)]
    pub cmd: Cmd,
}

#[derive(CmdLnArgsParser, Debug)]
enum Cmd {
    /// Run a Rex file
    #[command(about = "Run Rex code")]
    Run {
        /// Rex interface file that defines function stubs
        #[clap(short, long)]
        interf: Option<PathBuf>,

        /// Rex input file
        input: PathBuf,
    },
}

#[tokio::main]
pub async fn main() -> anyhow::Result<()> {
    tracing_subscriber::registry()
        .with(EnvFilter::new(
            env::var("TRACE").unwrap_or("WARN".to_string()),
        ))
        .with(fmt::layer().with_writer(io::stderr))
        .init();

    let cmd_ln_args = CmdLnArgs::parse();

    match cmd_ln_args.cmd {
        Cmd::Run { interf, input } => {
            let fn_forward_decls = match interf {
                Some(interf) => {
                    // Open the interface face
                    let mut interf = File::open(interf).await.context("opening interface file")?;
                    let mut interf_content = String::new();
                    interf
                        .read_to_string(&mut interf_content)
                        .await
                        .context("reading interface file")?;

                    // Parse forward function declarations so we can use them as stubs
                    let mut parser = Parser::new(
                        Token::tokenize(&interf_content).context("tokenizing interface")?,
                    );

                    let mut fn_forward_decls = vec![];
                    loop {
                        match parser.parse_fn_forward_decl() {
                            Ok((fn_ident, type_vars)) => {
                                fn_forward_decls.push((
                                    fn_ident,
                                    type_vars
                                        .into_iter()
                                        .map(|tv| rex_hmts::resolve_type_var(&tv))
                                        .collect::<Vec<_>>(),
                                ));
                            }
                            Err(_e) => {
                                break;
                            }
                        }
                    }
                    fn_forward_decls
                }
                None => vec![],
            };

            println!("Interface:\n  {:?}", fn_forward_decls);
            // Open the input file

            let mut input = File::open(input).await.context("opening input file")?;
            let mut input_content = String::new();
            input
                .read_to_string(&mut input_content)
                .await
                .context("reading input file")?;

            // Parse and evaluate the input file

            let mut parser = Parser::new(Token::tokenize(&input_content).context("tokenizing")?);
            let expr = parser.parse_expr().context("parsing")?;

            let mut id_dispenser = parser.id_dispenser;
            let ftable = Ftable::with_intrinsics(&mut id_dispenser);

            // Initialize the context with randomly generated values for
            // function stubs in the interface
            let mut ctx = Context::new();
            let mut scope = ftable.scope();

            for (fn_ident, fn_params) in fn_forward_decls {
                if fn_params.len() == 0 {
                    panic!("interface function never returns");
                }
                if fn_params.len() == 1 {
                    let id = id_dispenser.next();
                    let val = rex_hmts::gen_random_value(&mut id_dispenser, &fn_params[0]);
                    ctx.vars = ctx.vars.insert(id, val);
                    scope.vars.insert(fn_ident, id);
                    continue;
                }

                let mut arrow = Type::Arrow(
                    Box::new(fn_params[0].clone()),
                    Box::new(fn_params[1].clone()),
                );
                for i in 2..fn_params.len() {
                    match arrow {
                        Type::Arrow(a, b) => {
                            arrow = Type::Arrow(
                                a,
                                Box::new(Type::Arrow(b, Box::new(fn_params[i].clone()))),
                            );
                        }
                        _ => unreachable!(),
                    }
                }

                let id = id_dispenser.next();
                let val = rex_hmts::gen_random_value(&mut id_dispenser, &arrow);
                ctx.vars = ctx.vars.insert(id, val);
                scope.vars.insert(fn_ident, id);
            }

            let ast =
                rex_resolver::resolve(&mut id_dispenser, &mut scope, expr).context("resolving")?;
            let result = rex_engine::eval(&ctx, &ftable, &(), ast).await;

            match result {
                Ok(val) => println!("{}", val),
                Err(e) => println!(
                    "Error:\n  evaluating\n\nCaused by:\n  {}\n\nTrace:\n{}",
                    e,
                    sprint_trace_with_ident(e.trace(), "  ")
                ),
            }
        }
    }

    Ok(())
}
