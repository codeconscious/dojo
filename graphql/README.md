# graphql-dojo

I came across a chance to introduce myself to GraphQL, and I must say I've found it an interesting way to set up APIs.

This repository just contains some simple testing using JavaScript and Express to familiarize myself with the basics.

## Usage

Ensure Express is installed (use `npm install express` if not), then run `node server.js`.

## Sample queries
```
{
  people {
    id
    civilianName
    superName
    team {
      id
      name
    }
  }
}
```

```
{
  person(id: 2) {
    superName
    team {
      id
      name
    }
  }
}
```
