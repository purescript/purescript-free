/* jshint node: true */
"use strict";

var gulp = require("gulp");
var plumber = require("gulp-plumber");
var purescript = require("gulp-purescript");
var rimraf = require("rimraf");
var run = require("gulp-run");

var sources = [
  "src/**/*.purs",
  "test/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "src/**/*.js",
  "test/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("clean-docs", function (cb) {
  rimraf("docs", cb);
});

gulp.task("clean-output", function (cb) {
  rimraf("output", cb);
});

gulp.task("clean", ["clean-docs", "clean-output"]);

gulp.task("make", function() {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscMake({ ffi: foreigns }));
});

gulp.task("docs", ["clean-docs"], function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscDocs({
      docgen: {
        "Control.Comonad.Cofree": "docs/Control.Comonad.Cofree.md",
        "Control.Monad.Free": "docs/Control.Monad.Free.md",
        "Control.Monad.Trampoline": "docs/Control.Monad.Trampoline.md",
        "Data.Coyoneda": "docs/Data.Coyoneda.md",
        "Data.Yoneda": "docs/Data.Yoneda.md"
      }
    }));
});

gulp.task("dotpsci", function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.dotPsci());
});

gulp.task("test", function() {
  return gulp.src(sources.concat(["test/**/*.purs", "examples/**/*.purs"]))
    .pipe(plumber())
    .pipe(purescript.psc({ main: "Test.Main", ffi: foreigns }))
    .pipe(run("node"));
});

gulp.task("default", ["make", "docs", "dotpsci", "test"]);
