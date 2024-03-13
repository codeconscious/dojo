module Records

// https://app.pluralsight.com/ilx/video-courses/d6232165-f81d-4df7-b464-32f73de1091e/cee447d8-1e22-4b69-acb9-fd5be7c0936d/1484a99b-6216-494c-b2aa-84503d791754
type Person = { FirstName: string; LastName: string; Age: int }
    with member this.FullName = $"{this.FirstName} {this.LastName}"
         member this.IsSenior = this.Age >= 65
         member this.IsOlderThan age = this.Age > age
         static member New f l a = { FirstName = f; LastName = l; Age = a }
