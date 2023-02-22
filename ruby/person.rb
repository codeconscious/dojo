class Person
    @@count = 0

    def initialize(name:, supername:, powers:, team:)
        @name = name
        @supername = supername
        @powers = powers # An array might be better
        @team = team
        @@count += 1
    end

    attr_reader :name, :supername, :powers

    # def team=(newTeam)
    #     @team = newTeam
    # end
    attr_writer :team

    def Person.count
        @@count
    end

    def describe
        "#{@supername} (#{@name}) has the power of #{@powers} and is a member of #{@team}."
    end
end
