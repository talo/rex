use std::{env, io, path::PathBuf, sync::Arc};

use anyhow::Context as _;
use clap::Parser as CmdLnArgsParser;
use rex_ast::expr::Scope;
use rex_engine::{
    engine::Builder,
    eval::{eval, Context},
};
use rex_lexer::Token;
use rex_parser::Parser;
use rex_type_system::{constraint::generate_constraints, types::ExprTypeEnv, unify};
use tokio::{fs::File, io::AsyncReadExt, sync::RwLock};
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
        /// Rex input file
        #[clap(short, long)]
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
        Cmd::Run { input } => {
            let mut input = File::open(input).await.context("opening input file")?;
            let mut input_content = String::new();
            input
                .read_to_string(&mut input_content)
                .await
                .context("reading input file")?;

            let mut parser = Parser::new(Token::tokenize(&input_content).unwrap());
            let expr = parser.parse_expr().unwrap();

            let builder = Builder::with_prelude().unwrap();
            let (mut constraint_system, ftable, type_env) = builder.build();

            let mut expr_type_env = ExprTypeEnv::new();
            let ty =
                generate_constraints(&expr, &type_env, &mut expr_type_env, &mut constraint_system)
                    .unwrap();

            let subst = unify::unify_constraints(&constraint_system).unwrap();
            let _res_type = unify::apply_subst(&ty, &subst);

            let res = eval(
                &Context {
                    scope: Scope::new_sync(),
                    ftable,
                    subst,
                    env: Arc::new(RwLock::new(expr_type_env)),
                    state: (),
                },
                &expr,
            )
            .await;

            match res {
                Ok(val) => println!("{}", val),
                Err(e) => println!("Error:\n  evaluating\n\nCaused by:\n  {}", e,),
            }
        }
    }

    Ok(())
}
