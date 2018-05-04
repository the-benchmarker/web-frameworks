exports.files = {
  javascripts: {
    joinTo: "js/app.js"
  },
  stylesheets: {
    joinTo: "css/app.css"
  },
  templates: {
    joinTo: "js/app.js"
  }
}

exports.conventions = {
  assets: /^(static\/assets)/
}

exports.paths = {
  watched: [
    "static",
  ],

  public: "public"
}

exports.plugins = {
  babel: {
    ignore: [/static\/vendor/]
  },
  postcss: {
    processors: [
      require("precss"),
      require("lost"),
      require("postcss-cssnext")(),
      require("postcss-sass-color-functions")
    ]
  }
}

exports.modules = {
  autoRequire: {
    "js/app.js": ["static/js/app"]
  }
}

exports.npm = {
  enabled: true,
  globals: {$: "jquery", jQuery: "jquery"},
}
