import Benchmark from "benchmark";
import { mkdirSync, writeFileSync } from "node:fs";

// Benchotron disabled decompilation because generated PureScript functions
// close over imported modules. Keep its Benchmark.js defaults and batch timing.
export const runSuiteImpl = map => suite => () => {
  Benchmark.support.decompilation = false;
  mkdirSync("tmp", { recursive: true });
  for (const benchmark of suite) {
    const series = benchmark.functions.map(fn => ({ name: fn.name, results: [] }));
    for (const size of benchmark.sizes) {
      console.error(`${benchmark.slug}: n=${size}`);
      // Generate the same shared batch before measuring any competitor.
      const inputs = Array.from({ length: benchmark.inputsPerSize }, () => benchmark.gen(size));
      for (let i = 0; i < benchmark.functions.length; i++) {
        const fn = benchmark.functions[i];
        const measured = new Benchmark(() => map(fn.fn)(inputs));
        measured.run();
        if (measured.error) {
          throw new Error(`While running ${fn.name} at n=${size}: ${measured.error}`);
        }
        series[i].results.push({ size, stats: measured.stats });
      }
    }
    const result = {
      slug: benchmark.slug,
      title: benchmark.title,
      sizeInterpretation: benchmark.sizeInterpretation,
      series
    };
    const path = `tmp/${benchmark.slug}.json`;
    writeFileSync(path, JSON.stringify(result));
    console.error(`Results written to ${path}`);
  }
};
