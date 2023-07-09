type architecture = Jasmin.Glob_options.architecture

let architecture_of_yojson = function
  | `String "x86-64" -> Jasmin.Glob_options.X86_64
  | `String "arm-m4" -> Jasmin.Glob_options.ARM_M4
  | _ -> raise (Invalid_argument "architecture_of_yojson")

let yojson_of_architecture = function
  | Jasmin.Glob_options.X86_64 -> `String "x86-64"
  | Jasmin.Glob_options.ARM_M4 -> `String "arm-m4"

type source_module = {
  path: string;
  architecture: architecture;
} [@@deriving yojson]

type sources_data = {
  root: string;
  modules: source_module list option; [@yojson.option]
} [@@deriving yojson]

type project_data = {
  project_name: string;
  sources: sources_data;
} [@@deriving yojson]

let parse_project_file ~fname =
  Yojson.Safe.(from_file fname |> project_data_of_yojson)