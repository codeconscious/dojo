const message : string = "Hello World.";

function write(text : string) {
    console.log(text);
}

write(message);


let person : {
    id: number,
    name: string
};

person = {
    id: 100,
    name: "Yo"
}

// Array of two types
let peeps : (number | string)[] = [1, 2, "three", 5];
for (const p of peeps) {
    write(p.toString());
}

// Tuple
var city: [string, string] = ["Nagoya", "Aichi"];
write(`The first city is ${city[0]}.`);

// Enum
enum Character {
    Mario = "Mario", Luigi = "Luigi", Toad = "Toad", Peach = "Peach"
}

function isCharacterMarioBrother(character : Character) : string {
    if (character === Character.Mario || character === Character.Luigi) {
        return `${character} IS a Mario brother!`;
    }
    return `${character} is NOT a Mario brother.`;
}

write(`${isCharacterMarioBrother(Character.Luigi)}`);

// Union types
let v : (boolean | Character);
v = false;
v = Character.Toad;
//v = 1; // Fails

// Type assertion
let code: any = 123;
let employeeCode = code as number; // Same as <number> code;

// For of == foreach
// For in == accesses indexes, not values!


// Interfaces
interface IPerson {
    intelligence: number,
    age: number,
    readonly name: string,
    job?: string
};

interface IResident extends IPerson {
    location: string;
}

var bruce : IResident = {
    intelligence: 200,
    age: 35,
    name: "Bruce",
    job: "detective",
    location: "Gotham"
};

function printPerson(person : IResident) : void {
    let output = `${person.name} is ${person.age} with an IQ of about ${person.intelligence}.`;
    if (person.job !== undefined) {
        output += ` ${person.name} works as a ${person.job}.`;
    }
    if (person.location !== undefined) {
        output += ` ${person.name} lives in ${person.location}.`;
    }
    write(output);
}

// Method interface
interface IPersonPrinter {
    (person: IPerson) : void;
}

let printer : IPersonPrinter = printPerson;

printer(bruce);


// Modules
import * as Splatoon from  "./splatoonPlayer"
let j = new Splatoon.SplatoonPlayer("Ｊョースター", Splatoon.Gender.Boy, 81, Splatoon.GachiRank.A, "shooters");
let a = new Splatoon.SplatoonPlayer("Aaazzzz", Splatoon.Gender.Girl, 120, Splatoon.GachiRank.B, "shooters");
j.describe();
a.describe();


// Generics
function sortAndShowThings<T>(things : T[]) : void {
    let sortedThings = things.sort();
    console.log(sortedThings.join(", "));
}

let nums : number[] = [ 40, 6, 89, 103, -3 ];
let strs : string[] = [ "xa", "4j", "-3b", "gg", "!?" ];

sortAndShowThings(nums);
sortAndShowThings(strs);