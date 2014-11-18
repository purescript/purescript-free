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
    docgen: {
        readme: {
            src: "src/**/*.purs",
            dest: "README.md"
        }
    },

    psc: {
      options: {
        main: "Teletype"
      },
      example: {
        src: ["examples/Teletype.purs", "<%=libFiles%>"],
        dest: "tmp/Teletype.js"
      }
    },

    execute: {
      example: {
        src: "tmp/Teletype.js"
      }
    }

  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");
  grunt.loadNpmTasks("grunt-contrib-clean");

  grunt.registerTask("example", ["psc", "execute"]);
  grunt.registerTask("make", ["pscMake", "dotPsci", "docgen"]);
  grunt.registerTask("default", ["make"]);
};
