def scheme_source(name, src):
  base_name = src[:-4]
  js_name = base_name + ".js"
  native.genrule(
      name = base_name,
      srcs = [src],
      outs = [js_name],
      tools = [
          "//src/java/r5js:SourceWriter",
      ],
      cmd = "$(location //src/java/r5js:SourceWriter) $< " + name + " > $@",
  )