load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

def scheme_source(name, src):
  base_name = src[:-4]
  ts_name = base_name + ".ts"

  # TODO investigate using `template strings`. Would be nice to ship the sources non-mangled.
  native.genrule(
      name = name + "_src",
      srcs = [src],
      outs = [ts_name],
      cmd = "cat > $(@) << END\n"
      + "export const " + name + " = \n"
      + "END\n"
      + "cat $(<)"
      # backslash-escape backslash
      + " | sed -e 's/\\\\/\\\\\\\\/g'"
      # backslash-escape double quotes
      + " | sed -e 's/\"/\\\\\"/g'"
      # newline => \n (two characters)
      + " | sed -e 's/$$/END_OF_LINE/g'"
      + " | tr '\\n' ' '"
      # leading quote
      + " | sed -e 's/^/\"/'"
      # trailing quote and semicolon
      + " | sed -e 's/$$/\";/'"
      + " | sed -e 's/END_OF_LINE/\\\\n/g'"
      + " >> $(@)",
  )

  ts_library(
      name = name,
      tsconfig = "//:tsconfig.json",
      srcs = [
          ":" + name + "_src",
      ],
      visibility = [
          "//:__subpackages__",
      ],
  )