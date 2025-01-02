function bar(s: string): number {
  return 54;
}
function foo(a: number = 7, b: string): number {
  return bar(a + b);
}