"use strict";
exports.__esModule = true;
exports.GachiRank = exports.Gender = exports.SplatoonPlayer = void 0;
var SplatoonPlayer = /** @class */ (function () {
    // Is there a more succinct syntax?
    function SplatoonPlayer(name, gender, level, gachiRank, weaponType) {
        this.name = name;
        this.gender = gender;
        this.level = level;
        this.gachiRank = gachiRank;
        this.preferredWeaponType = weaponType;
    }
    SplatoonPlayer.prototype.describe = function () {
        console.log(this.name + " is a " + this.gender + " (level " + this.level + ", rank " + this.gachiRank + ") that likes " + this.preferredWeaponType + ".");
    };
    return SplatoonPlayer;
}());
exports.SplatoonPlayer = SplatoonPlayer;
var Gender;
(function (Gender) {
    Gender["Boy"] = "boy";
    Gender["Girl"] = "girl";
})(Gender = exports.Gender || (exports.Gender = {}));
var GachiRank;
(function (GachiRank) {
    GachiRank["C"] = "C";
    GachiRank["B"] = "B";
    GachiRank["A"] = "A";
    GachiRank["S"] = "S";
    GachiRank["X"] = "X";
})(GachiRank = exports.GachiRank || (exports.GachiRank = {}));
