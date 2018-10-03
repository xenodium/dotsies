#name : Shell script arguments
#key : shellArgs
# --
struct Script {
  static let fileName = CommandLine.arguments[0]

  struct Arguments {
    let $1: $2
  }

  static func arguments() -> Arguments? {
    if CommandLine.arguments.count < 2 {
      return nil
    }
    return Arguments($1: CommandLine.arguments[1])
  }
}

$0
