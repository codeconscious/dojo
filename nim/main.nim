import std/strformat
from std/strutils import join

type
    Hero = object
        name: string
        team: seq[string]
        powers: seq[string]

let heroes = [
    Hero(name: "Cyclops", team: @["X-Men"], powers: @["optic blasts"]),
    Hero(name: "Marvel Girl", team: @["X-Men"], powers: @["telepathy", "telekinesis"]),
    Hero(name: "Beast", team: @["X-Men", "Avengers"], powers: @["enhanced strength", "enhanced agility"]),
]

for hero in heroes:
    let team = hero.team.join(", ")
    let powers = hero.powers.join(" and ")
    echo(fmt"• {hero.name} ({team}) has {powers}.")
