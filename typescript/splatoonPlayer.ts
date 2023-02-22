export class SplatoonPlayer {
    name : string;
    gender : Gender;
    level : number;
    gachiRank : GachiRank;
    preferredWeaponType : string;

    // Is there a more succinct syntax?
    constructor(name : string, gender : Gender, level : number, gachiRank : GachiRank, weaponType : string) {
        this.name = name;
        this.gender = gender;
        this.level = level;
        this.gachiRank = gachiRank;
        this.preferredWeaponType = weaponType;
    }

    describe() : void {
        console.log(`${this.name} is a ${this.gender} (level ${this.level}, rank ${this.gachiRank}) that likes ${this.preferredWeaponType}.`)
    }
}

export enum Gender {
    Boy = "boy",
    Girl = "girl"
}

export enum GachiRank {
    C = "C",
    B = "B",
    A = "A",
    S = "S",
    X = "X"
}