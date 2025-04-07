use std::{env, io, path::PathBuf};

use anyhow::Context as _;
use clap::Parser as CmdLnArgsParser;
use rex::engine::{engine::Builder, program::Program};
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

            let builder = Builder::with_prelude().unwrap();
            let program = Program::compile(builder, &input_content).unwrap();
            let res = program.run(()).await;

            match res {
                Ok(val) => println!("{}", val),
                Err(e) => println!("Error:\n  evaluating\n\nCaused by:\n  {}", e,),
            }
        }
    }

    Ok(())
}
