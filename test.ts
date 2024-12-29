var x = 1;
// single line comment
x = 2;
/* multiple lines
   comment
   /* cannot be nested
    -> ends here */
x = 3;
   
var x1 : number = 0;
let x2 : number = 42;
const x3 : number = 1_02_4;
var x4 : 42 = 0b101010;
let x5 : number = 0B1_0_10_1;
var x6 = 0o1234_5670;
let x7 = 0O071;
const x8 = 0x01_f32; 
let x9 = 0x01_e3;  
var x10 : number = 0XDead_Beef;
var x11 : number = 3.14159;
let x12 = 1.0_1e+24;
const x13 = 9.e-01;
var x14 : number = .123;
let x15 = .1e1;
let x16 = 2_4e1_2;
var x17 : boolean = true;
let x18 : false = false;
const x19 : string = "coucou";
const x20 : "blabla" = 'blabla';
var x21 = '" escaped \' here';
let x22 = "bla'bla\"bli\"bla";
var x23 = "";

const x : { p : number } = { p : 1 };
x.p = 2;

const t = [1,2,3];
t[1] = 0;

type numerr = number | "error";

let x : number = 1, y : string, z = 12;
const e = 1, f : string = "d";

function fonction(u : string, v : number = 42, w) : number {
    return v;
}

function nope() {
    return;
}

x = fonction(y, 12, 1);


var x;
x = [1, 2, 3];
x[2] = 0;
x = { y : 1, z : 0 };
x.y = 42;
x = [{ z : 2, h : 1 }.z, 12, 0];
x = typeof (-12);
x = 12 <= 3 * 2 ** 8 - 1 + 4 / 1.2 && 4 > -2 || x == "number";


;
42 - 1;
{
  let x : number = 1;
  var y : string, z = 12, h;
}
y = "coucou";
if (y != "bad")
  if (y != "good")
    y = "bla";
  else
    y = "bli";
while (true)
  y = y + "s";

y = 2;

var y : number;

function f() {
    z = "oucou";
    { let x = 1;
      var z : string;
      x = 2;
    }
}

y = g(2);

function g(a : number) : number {
    return a + 1;
}


let x1 : number = 42;
let x2 : string | number = 12.2;
let x3 : any = [1, 3, 12];

function f(x : number | "coucou" | string[]) {
}

f(42);
f("coucou");
f(["", "1"]);


type square = { color : string;		side : number };

type circle = { radius : number, color : string, };

// function color_of(h : square | circle) : string {
//     return h.color;
// }

var r : string;

r = color_of({ color : "blue", side : 2 });
r = color_of({ radius : 1.2, color : "red"});


var x : 2;
let y : true;
var z : "coucou";
var x1 : number;
let x2 : boolean;
var x3 : string;
var x4 : number[];
let x5 : boolean[][];
let x6 : any;
let x7 : { x : number; y, z : string };
let x8 : number | "error";
let x9 : string | boolean | number[];