using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading.Tasks;
using static System.Console;

namespace CSharp
{
    public static class RestAPI
    {
        static readonly HttpClient Client = new()
        {
            BaseAddress = new Uri("https://localhost:5001"),
        };

        public static void Start()
        {
            // Necessary to prevent the program from closing prematurely.
            GetPeople().Wait();
            //GetPerson(1).Wait();
        }

        public async static Task GetPerson(int id)
        {
            if (id < 0)
            {
                WriteLine($"Failure: {System.Net.HttpStatusCode.BadRequest} - Invalid ID given");
                return;
            }

            // Add an Accept header for JSON format.
            Client.DefaultRequestHeaders.Accept.Add(
                new MediaTypeWithQualityHeaderValue("application/json")
            );

            HttpResponseMessage response;
            try
            {
                response = await Client.GetAsync("/person/" + id);
            }
            catch (Exception ex)
            {
                WriteLine("Network error: " + ex.Message);
                return;            }


            if (!response.IsSuccessStatusCode)
            {
                WriteLine($"Failure: {(int)response.StatusCode} - {response.ReasonPhrase}");
                return;
            }

            WriteLine("Success!");
            var data = await JsonSerializer.DeserializeAsync<PersonDto>(response.Content.ReadAsStream());

            if (data == null)
            {
                WriteLine($"Failure: {(int)response.StatusCode} - {response.ReasonPhrase}");
                return;
            }

            WriteLine($"  * {data.Name} (#{data.Id}) is {data.Age} years old and from {data.Birthplace}");
        }

        public async static Task GetPeople()
        {
            // Add an Accept header for JSON format.
            Client.DefaultRequestHeaders.Accept.Add(
                new MediaTypeWithQualityHeaderValue("application/json")
            );

            HttpResponseMessage response;
            try
            {
                response = await Client.GetAsync("/people");
            }
            catch (HttpRequestException ex)
            {
                WriteLine("Network error: " + ex.Message);
                return;
            }

            if (!response.IsSuccessStatusCode)
            {
                WriteLine($"Failure: {response.StatusCode} - {response.ReasonPhrase}");
                return;
            }

            WriteLine("Success!");
            var data = await JsonSerializer.DeserializeAsync<List<PersonDto>>(response.Content.ReadAsStream());

            if (data == null)
            {
                WriteLine("Null.");
                return;
            }

            if (!data.Any())
            {
                WriteLine("None.");
                return;
            }

            data.ForEach(d => WriteLine($"  * {d.Name} (#{d.Id}) is {d.Age} years old and from {d.Birthplace}"));
        }

        public record PersonDto
        {
            [JsonPropertyName("id")]
            public int Id { get; init; }

            [JsonPropertyName("name")]
            public string? Name { get; init; }

            [JsonPropertyName("age")]
            public int Age { get; init; }

            [JsonPropertyName("birthplace")]
            public string? Birthplace { get; init; }

            public PersonDto(int id, string name, int age, string birthplace)
            {
                Id = id;
                Name = name;
                Age = age;
                Birthplace = birthplace;
            }
        }
    }
}
