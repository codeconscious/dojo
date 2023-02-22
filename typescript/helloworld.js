"use strict";
exports.__esModule = true;
var message = "Hello World.";
function write(text) {
    console.log(text);
}
write(message);
var person;
person = {
    id: 100,
    name: "Yo"
};
// Array of two types
var peeps = [1, 2, "three", 5];
for (var _i = 0, peeps_1 = peeps; _i < peeps_1.length; _i++) {
    var p = peeps_1[_i];
    write(p.toString());
}
// Tuple
var city = ["Nagoya", "Aichi"];
write("The first city is " + city[0] + ".");
// Enum
var Character;
(function (Character) {
    Character["Mario"] = "Mario";
    Character["Luigi"] = "Luigi";
    Character["Toad"] = "Toad";
    Character["Peach"] = "Peach";
})(Character || (Character = {}));
function isCharacterMarioBrother(character) {
    if (character === Character.Mario || character === Character.Luigi) {
        return character + " IS a Mario brother!";
    }
    return character + " is NOT a Mario brother.";
}
write("" + isCharacterMarioBrother(Character.Luigi));
// Union types
var v;
v = false;
v = Character.Toad;
//v = 1; // Fails
// Type assertion
var code = 123;
var employeeCode = code; // Same as <number> code;
;
var bruce = {
    intelligence: 200,
    age: 35,
    name: "Bruce",
    job: "detective",
    location: "Gotham"
};
function printPerson(person) {
    var output = person.name + " is " + person.age + " with an IQ of about " + person.intelligence + ".";
    if (person.job !== undefined) {
        output += " " + person.name + " works as a " + person.job + ".";
    }
    if (person.location !== undefined) {
        output += " " + person.name + " lives in " + person.location + ".";
    }
    write(output);
}
var printer = printPerson;
printer(bruce);
// Modules
var Splatoon = require("./splatoonPlayer");
var j = new Splatoon.SplatoonPlayer("Ｊョースター", Splatoon.Gender.Boy, 81, Splatoon.GachiRank.A, "shooters");
var a = new Splatoon.SplatoonPlayer("Aaazzzz", Splatoon.Gender.Girl, 120, Splatoon.GachiRank.B, "shooters");
j.describe();
a.describe();
// Generics
function sortAndShowThings(things) {
    var sortedThings = things.sort();
    console.log(sortedThings.join(", "));
}
var nums = [40, 6, 89, 103, -3];
var strs = ["xa", "4j", "-3b", "gg", "!?"];
sortAndShowThings(nums);
sortAndShowThings(strs);
