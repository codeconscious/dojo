export class SplatoonPlayer {
    name: string;
    gender: Gender;
    level: number;
    gachiRank: GachiRank;
    preferredWeaponType: WeaponType;

    // Is there a shorter syntax?
    constructor(name: string, gender: Gender, level: number, gachiRank: GachiRank, weaponType: WeaponType) {
        this.name = name;
        this.gender = gender;
        this.level = level;
        this.gachiRank = gachiRank;
        this.preferredWeaponType = weaponType;
    }

    describe(): void {
        console.log(`${this.name} is a ${this.gender} (level ${this.level}, rank ${this.gachiRank}) that likes ${this.preferredWeaponType}.`)
    }
}

enum Gender {
    Boy = "boy",
    Girl = "girl"
}

enum GachiRank {
    C = "C",
    B = "B",
    A = "A",
    S = "S",
    X = "X"
}

enum WeaponType {
    Shooters = "Shooters",
    Rollers = "Rollers",
    Chargers = "Chargers",
    Sloshers = "Sloshers",
    Splatlings = "Splatlings",
    Dualies = "Dualies",
    Brellas = "Brellas",
    Blasters = "Blasters",
    Brushes = "Brushes",
    Stringers = "Stringers",
    Splatanas = "Splatanas",
}
