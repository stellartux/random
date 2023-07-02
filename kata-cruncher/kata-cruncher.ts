import { parse } from "https://deno.land/std@0.145.0/flags/mod.ts";
import { readAll } from "https://deno.land/std@0.145.0/streams/conversion.ts";
import { from, to } from "./main.js";

async function cli(): Promise<void> {
  const extToLang: Record<string, string> = {
    js: "JavaScript",
    jl: "Julia",
    lisp: "Common Lisp",
    lua: "Lua",
    ts: "JavaScript",
  };

  const args = parse(Deno.args);

  if (args.help || args.h) {
    console.log(`
    
        Kata Cruncher - the test case conversion tool for CodeWars
        usage: kata-cruncher [options] [filenames...]
    
    Options:
    -h, --help
        Show this help message.
    --to='${[...Object.values(extToLang)].join("','")}'
        Choose the output language. Defaults to 'Julia'. 
        Short versions of the language name may also be used.
    `);
    return;
  }

  const decoder = new TextDecoder("utf-8");

  if (!args.to) {
    args.to = "Julia";
  } else if (args.to.toLowerCase?.() in extToLang) {
    args.to = extToLang[args.to.toLowerCase()];
  }

  if (args._.length === 0) {
    console.log(
      to[args.to](from.JavaScript(decoder.decode(await readAll(Deno.stdin)))),
    );
  } else {
    for (const filename of args._) {
      console.log(
        to[args.to](decoder.decode(Deno.readFileSync(filename as string))),
      );
    }
  }
}

if (import.meta.main) cli();
