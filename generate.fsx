open System
open System.IO
open System.Text

type Company =
    {
        Url : string
        LogoPath : string
    }

type SponsorElement =
    | Person of name : string
    | Company of company : Company

type Information =
    {
        Sponsors : SponsorElement list
        GenerousBackers : SponsorElement list
        Backers : SponsorElement list
        TeaSupporters : SponsorElement list
        PaypalDonors : SponsorElement list
    }

[<Literal>]
let PAGE_WIDTH = 800

[<Literal>]
let PAGE_TOP_MARGIN = 80

[<Literal>]
let HEADER_BOTTOM_MARGIN = 50

[<Literal>]
let VERTICAL_MARGIN_BETWEEN_TABLE = 20

[<Literal>]
let MARGIN_BETWEEN_TABLE_ROWS = 30

let sponsorsInformation =
    {
        Sponsors =
            [
                Person "Joh Dokler"
            ]
        GenerousBackers =
            [
                Person "My parents"
                Person "TheWhitetigle"
                Company {
                    Url = "https://www.compositional-it.com/"
                    LogoPath = "./logos/compositional-it.png"
                }
            ]
        Backers =
            [
                Person "Paweł Stadnicki"
                Person "David Dawkins"
                Person "Vagif Abilov"
                Person "Mickael Metesreau"
                Person "Philip Nguyen"
                Person "Michael Fremont"
            ]
        TeaSupporters =
            [
                Person "Florian Verdonck"
                Person "Jan Bizub"
                Person "Maximilian Wilson"
                Person "Onur Gumus"
            ]
        PaypalDonors =
            [
                Person "George Danila"
                Person "Marek Simander"
                Person "Tomas Leko"
            ]
    }

// Allow us to have syntax highlighting thanks to VS Code extension
// https://marketplace.visualstudio.com/items?itemName=alfonsogarciacaro.vscode-template-fsharp-highlight
// We are generating SVG, but the extension is made for HTML orginally
// The name of the function doesn't matter as this function does nothing
let inline html s = s

let generate (height : int) (content : string) =
    html $"""
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="{PAGE_WIDTH}" height="{height}">
    <style>
        svg {{
            background-color: white;
        }}
        a {{
            cursor: pointer;
        }}
        text {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
        }}
        .section-header {{
            font-size: 20px;
            font-weight: bold;
            fill: #777777;
        }}
        .sponsor-name {{
            font-size: 16px;
        }}
    </style>
    {content}
</svg>"""

let renderSectionHeader (builder : StringBuilder) (y : int ref) (title : string) =
    let svg =
        html $"""<text x="{PAGE_WIDTH / 2}" y="{y.Value}" text-anchor="middle" class="section-header">{title}</text>"""

    builder.AppendLine(svg) |> ignore

    y.Value <- y.Value + HEADER_BOTTOM_MARGIN

module Common =

    let renderPersonTable
        (config :
            {|
                Builder : StringBuilder
                Y : int ref
                Persons : string list
                CountPerRow : int
                PageMargin : int
            |}
        ) =

        let mutable col = 0
        let mutable row = 0

        let personTableColumnWidth =
            (PAGE_WIDTH - config.PageMargin * 2) / config.CountPerRow

        let svg =
            [
                for name in config.Persons do
                    let x = config.PageMargin + col * personTableColumnWidth
                    let y = config.Y.Value + row * MARGIN_BETWEEN_TABLE_ROWS
                    html $"""
                        <text
                            x="{x}"
                            y="{y}"
                            class="sponsor-name">
                            {name}
                        </text>
                    """

                    // Increase column for next element
                    col <- col + 1
                    // If we reached the end of the row, reset the column and increase the row
                    if col >= 3 then
                        col <- 0
                        row <- row + 1
            ]
            |> String.concat "\n"

        config.Builder.AppendLine(svg) |> ignore

        config.Y.Value <- config.Y.Value + row * MARGIN_BETWEEN_TABLE_ROWS

    let renderCompanyTable
        (config :
            {|
                Builder : StringBuilder
                Y : int ref
                Companies : Company list
                CountPerRow : int
                PageMargin : int
                LogoWidth : int
                LogoHeight : int
            |}
        ) =

        let mutable col = 0
        let mutable row = 0

        let columnMargin =
            (
                PAGE_WIDTH // Global page width
                - config.PageMargin * 2 // Left and right margins
                - config.LogoWidth * config.CountPerRow // X logos per column
            ) / (config.CountPerRow - 1) // (X - 1) margins

        let svg =
            [
                for company in config.Companies do
                    let logoPath = System.IO.Path.Join(__SOURCE_DIRECTORY__, company.LogoPath)
                    let bytes = File.ReadAllBytes(logoPath)
                    let base64Representation = Convert.ToBase64String(bytes)

                    let x = config.PageMargin + col * (config.LogoWidth + columnMargin)
                    let y = config.Y.Value + row * config.LogoHeight

                    html $"""
                        <a
                            xlink:href="{company.Url}">
                            <image
                                x="{x}"
                                y="{y}"
                                width="{config.LogoWidth}"
                                height="{config.LogoHeight}"
                                xlink:href="data:image/png;base64,{base64Representation}"/>
                        </a>
                    """

                    // Increase column for next element
                    col <- col + 1
                    // If we reached the end of the row, reset the column and increase the row
                    if col >= config.CountPerRow then
                        col <- 0
                        row <- row + 1

            ]
            |> String.concat "\n"

        config.Builder.AppendLine(svg) |> ignore

        config.Y.Value <- config.Y.Value + row * 50


    let extractPersonAndCompaniesInfo (elements : SponsorElement list) =

        let (persons, companies) =
            elements
            |> List.partition (function
                | Person _ -> true
                | Company _ -> false
            )

        let persons =
            persons
            |> List.map (function
                | Person name -> name
                | Company _ -> failwith "Expected Person"
            )

        let companies =
            companies
            |> List.map (function
                | Person _ -> failwith "Expected Company"
                | Company company ->
                    company
            )

        persons, companies

    let renderSection
        (config :
            {|
                Builder : StringBuilder
                Y : int ref
                Title : string
                Elements : SponsorElement list
                PersonPerRow : int
                PersonTablePageMargin : int
                CompanyPerRow : int
                CompanyTablePageMargin : int
                LogoWidth : int
                LogoHeight : int
            |}
        ) =

        // Sponsors
        renderSectionHeader config.Builder config.Y config.Title

        // Render persons
        let persons, companies =
            extractPersonAndCompaniesInfo config.Elements

        renderPersonTable
            {|
                Builder = config.Builder
                Y = config.Y
                Persons = persons
                CountPerRow = config.PersonPerRow
                PageMargin = config.PersonTablePageMargin
            |}

        // If a person's table and a company's table should be generated,
        // add a margin between them
        if not persons.IsEmpty && not companies.IsEmpty then
            config.Y.Value <- config.Y.Value + VERTICAL_MARGIN_BETWEEN_TABLE

        renderCompanyTable
            {|
                Builder = config.Builder
                Y = config.Y
                Companies = companies
                CountPerRow = config.CompanyPerRow
                PageMargin = config.CompanyTablePageMargin
                LogoWidth = config.LogoWidth
                LogoHeight = config.LogoHeight
            |}

        // Add bottom margin to end this section
        if not companies.IsEmpty then
            // If there are companies, we need to consider the logo height
            config.Y.Value <- config.Y.Value + config.LogoHeight + HEADER_BOTTOM_MARGIN
        else
            config.Y.Value <- config.Y.Value + HEADER_BOTTOM_MARGIN

// I am using mutable variables here, because it makes it easier to keep
// track of the context information.
// Originally I was using immutable variables,
// and recomputing the y position depending on the current
// section but it was more complicated to implement

// For a small script like that it doesn't have a big impact on the maintainability

// Keep track of the Y position
let y = ref PAGE_TOP_MARGIN // Initial offset

let svgBuilder = StringBuilder()

Common.renderSection
    {|
        Builder = svgBuilder
        Y = y
        Title = "Sponsors"
        Elements = sponsorsInformation.Sponsors
        PersonPerRow = 3
        PersonTablePageMargin = 150
        CompanyPerRow = 3
        CompanyTablePageMargin = 50
        LogoWidth = 200
        LogoHeight = 80
    |}

Common.renderSection
    {|
        Builder = svgBuilder
        Y = y
        Title = "Generous backers"
        Elements = sponsorsInformation.GenerousBackers
        PersonPerRow = 3
        PersonTablePageMargin = 150
        CompanyPerRow = 3
        CompanyTablePageMargin = 50
        LogoWidth = 175
        LogoHeight = 70
    |}


Common.renderSection
    {|
        Builder = svgBuilder
        Y = y
        Title = "Backers"
        Elements = sponsorsInformation.Backers
        PersonPerRow = 3
        PersonTablePageMargin = 150
        // No company logo here
        CompanyPerRow = 999999
        CompanyTablePageMargin = 0
        LogoWidth = 0
        LogoHeight = 0
    |}


Common.renderSection
    {|
        Builder = svgBuilder
        Y = y
        Title = "Tea supporters"
        Elements = sponsorsInformation.TeaSupporters
        PersonPerRow = 3
        PersonTablePageMargin = 150
        // No company logo here
        CompanyPerRow = 999999
        CompanyTablePageMargin = 0
        LogoWidth = 0
        LogoHeight = 0
    |}


Common.renderSection
    {|
        Builder = svgBuilder
        Y = y
        Title = "Paypal donors"
        Elements = sponsorsInformation.PaypalDonors
        PersonPerRow = 3
        PersonTablePageMargin = 150
        // No company logo here
        CompanyPerRow = 999999
        CompanyTablePageMargin = 0
        LogoWidth = 0
        LogoHeight = 0
    |}

let res =
    generate y.Value (svgBuilder.ToString())

File.WriteAllText("backers.svg", res)
