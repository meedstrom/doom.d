const katex = require("katex");

let input = process.argv[2].trim();
let disp = true;
if (input.slice(0, 2) === "\\[" || input.slice(0, 2) === "$$") {
  input = input.slice(2, -2);
} else if (input.slice(0, 2) === "\\(") {
  input = input.slice(2, -2);
  disp = false;
} else if (input.slice(0, 1) === "$") {
  input = input.slice(1, -1);
  disp = false;
} else {
  console.error("Did you quote the input correctly?");
  process.exit(1);
}
console.log(
  katex.renderToString(input, {
    displayMode: disp,
    output: "mathml",
    trust: true,
    // I'd love to see errors, but the org exporter doesn't show you the thrown
    // error anyway, it just inserts the error string into the document.
    // Better to return the original latex.
    throwOnError: false,
    strict: false,
  }),
);
