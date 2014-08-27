var gulp = require('gulp')
  , clean = require('gulp-clean')
  , gutil = require('gulp-util')
  , plumber = require('gulp-plumber')
  , purescript = require('gulp-purescript')
  , sequence = require('run-sequence')
  , config = {
      clean: ['dist', 'js', 'externs'],
      purescript: {
        src: [
          'bower_components/purescript-*/src/**/*.purs*',
          'src/**/*.purs'
        ],
        examples: 'examples/**/*.purs',
        docgen: 'README.md',
        dest: 'dist',
        options: {
          main: 'Teletype'
        }
      }
    }
;

function error(e) {
  gutil.log(gutil.colors.magenta('>>>> Error <<<<') + '\n' + e.toString().trim());
  this.emit('end');
}

gulp.task('clean', function(){
  return (
    gulp.src(config.clean, {read: false}).
      pipe(clean())
  );
});

gulp.task('examples', function(){
  return (
    gulp.src([config.purescript.examples].concat(config.purescript.src)).
    pipe(plumber()).
    pipe(purescript.psc(config.purescript.options)).
    on('error', error).
    pipe(gulp.dest(config.purescript.dest))
  );
});

gulp.task('make', function(){
  return (
    gulp.src(config.purescript.src).
    pipe(plumber()).
    pipe(purescript.pscMake({output: config.purescript.dest})).
    on('error', error)
  );
});

gulp.task('psci', function(){
  return (
    gulp.src(config.purescript.src).
    pipe(plumber()).
    pipe(purescript.dotPsci()).
    on('error', error)
  );
});

gulp.task('docgen', function(){
  return (
    gulp.src(config.purescript.src[1]).
    pipe(plumber()).
    pipe(purescript.docgen()).
    on('error', error).
    pipe(gulp.dest(config.purescript.docgen))
  );
});

gulp.task('watch', function(cb){
  gulp.watch(config.purescript.src, ['make']);
});

gulp.task('default', function(){
  sequence('clean', 'make', ['psci', 'docgen']);
});
