// Compile using `kotlinc main.kt -include-runtime -d main.jar` (長いね)
// Run the compiled file using `java -jar ./main.jar`
fun main() {
    println("Kotlinへようこそ！")

    val charles = Person("Charles", "Xavier")
    val profX = Mutant(charles, "telepathy", "Professor X")
    println(profX.description)

    val scott = Person("Scott", "Summers")
    val cyclops = Mutant(scott, "optic blasts", "Cyclops")
    println(cyclops.description)

    val cannonball = Mutant(Person("Samuel", "Guthrie"), "thermo-chemical blastin'", "Cannonball")
    println(cannonball.description)

    val xMen = Team("X-Men", listOf(profX, cyclops, cannonball))
    println(xMen.description)
}

class Person(val firstName: String, val lastName: String) {
    val fullName = "$firstName $lastName"
}

class Mutant(val person: Person, val power: String, val mutantName: String) {
    val description = "$mutantName (${person.fullName}) has the power of ${power}."
}

class Team(val name: String, val members: List<Mutant>) {
    val description = "The team $name consists of ${members.count()} member(s): ${members.map { it.mutantName }.joinToString(separator = " and ")}."
}
