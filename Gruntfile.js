module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs*"
    ],

    clean: ["output", "tmp"],

    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
    pscDocs: {
        readme: {
            src: "src/**/*.purs",
            dest: "README.md"
        }
    },

    psc: {
      teletypeExample: {
        options: {
          main: "Teletype"
        },
        src: ["examples/Teletype.purs", "<%=libFiles%>"],
        dest: "tmp/Teletype.js"
      },
      teletypeCoproductExample: {
        options: {
          main: "TeletypeCoproduct"
        },
        src: ["examples/TeletypeCoproduct.purs", "<%=libFiles%>"],
        dest: "tmp/TeletypeCoproduct.js"
      }
    },

    execute: {
      teletypeExample: {
        src: "tmp/Teletype.js"
      },
      teletypeCoproductExample: {
        src: "tmp/TeletypeCoproduct.js"
      }
    },

    jsvalidate: ["output/**/*.js"]

  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-execute");
  grunt.loadNpmTasks("grunt-jsvalidate");
  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("example", ["psc", "execute"]);
  grunt.registerTask("make", ["pscMake", "dotPsci", "pscDocs", "jsvalidate"]);
  grunt.registerTask("default", ["make"]);
};
