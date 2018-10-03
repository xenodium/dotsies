#name : Shell script arguments
#key : shellArgs
# --
struct Script {
  let fileName: String
  let arguments: Arguments

  struct Arguments {
    let $1: String
  }

  init() {
    fileName = CommandLine.arguments[0]

    if CommandLine.arguments.count < 2 {
      fputs("Usage: \\(fileName) $2", stderr)
      exit(1)
    }

    arguments = Arguments($1: CommandLine.arguments[1])
  }
}

$0
