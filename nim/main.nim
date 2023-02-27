import std/strformat
import std/sequtils
from std/strutils import join

type
    Team = object
        id: Natural
        name: string

type
    Hero = object
        name: string
        teams: seq[Team]
        powers: seq[string]

let team_xmen = Team(id: 1, name: "X-Men")
let team_avengers = Team(id: 2, name: "Avengers")

let heroes = [
    Hero(name: "Cyclops", teams: @[team_xmen], powers: @["optic blasts"]),
    Hero(name: "Marvel Girl", teams: @[team_xmen], powers: @["telepathy", "telekinesis"]),
    Hero(name: "Beast", teams: @[team_xmen, team_avengers], powers: @["enhanced strength", "enhanced agility"]),
]

for hero in heroes:
    let teams = map(hero.teams, proc(t: Team): string = t.name).join(", ") # BROKEN
    let powers = hero.powers.join(" and ")
    echo(fmt"• {hero.name} ({teams}) has {powers}.")
