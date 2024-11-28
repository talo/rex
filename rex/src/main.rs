use std::{env, io, path::PathBuf};

use anyhow::Context as _;
use clap::Parser as CmdLnArgsParser;
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
            // Open the interface face

            let _interf_content = match interf {
                Some(interf) => {
                    let mut interf = File::open(interf).await.context("opening interface file")?;
                    let mut interf_content = String::new();
                    interf
                        .read_to_string(&mut interf_content)
                        .await
                        .context("reading interface file")?;
                    Some(interf_content)
                }
                None => None,
            };

            // Open the input file

            let mut input = File::open(input).await.context("opening input file")?;
            let mut input_content = String::new();
            input
                .read_to_string(&mut input_content)
                .await
                .context("reading input file")?;

            // TODO: Parse the interface file

            // Parse and evaluate the input file

            let mut parser = Parser::new(Token::tokenize(&input_content).context("tokenizing")?);
            let expr = parser.parse_expr().context("parsing")?;

            let mut id_dispenser = parser.id_dispenser;
            let ftable = Ftable::with_intrinsics(&mut id_dispenser);

            let mut scope = ftable.scope();

            let ctx = Context::new();
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
