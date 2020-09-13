---
date: 2020-09-03
title: My Type-Safe Blog in Haskell
tags: 
 - haskell
---

Like most, my first step into the blogging world was through WordPress. But after a few years of wrangling plugins, handing endless updates, dealing with custom CSS issues, and paying for shared web hosting, the idea of using a static site generator seemed more attractive. Whereas WordPress sites are dynamically generated on a web server, static sites are just made up of good old HTML pages, and are therefore much simpler, more secure, and easier to maintain. Plus, you can serve those pages using a free service like GitHub pages. I switched to Jekyll.

But [my first step](https://github.com/JonathanReeve/jonreeve.com/tree/816a19ff4454313a27f14a5ba9c7f5a5a5fc2d11) into the world of static site generators quickly grew in complexity. Jekyll allows you to avoid writing HTML by writing in Markdown instead, which is a real improvement. And to avoid the tedium of writing CSS, you can write in Sass instead. Then there's Coffeescript, which compiles to JavaScript. You can template together all your pages using the templating language Liquid. But for each of these conveniences, there is some technical overhead. First, all of these micro-languages are compiled using a stack of Ruby gems. To manage those gems, you need Bundler, which puts them all together, and probably also a manager which juggles all your versions of Ruby. And in the end, you have to remember lots of syntax: what does a variable look like in Sass, again? What about Coffeescript? Liquid?

For my [second time around](https://github.com/JonathanReeve/jonreeve.com/tree/36b7520fb9c71ffc09b6eec3007994b1c32c3e01), I wanted to solve a problem with my CV, which was that I wanted to avoid repeating myself in situations where I had a publication related to a project that was listed in both the *Projects* section and the *Publications* section. So I decided to put all that data in a data format, YAML, and then display it using templates. To do all that, I chose the simplest tool I could find. [Metalsmith](https://metalsmith.io/) bills itself as "an extremely simple, pluggable static site generator." And it is. Everything in Metalsmith is a plugin, and so getting the perfect blog set up was just a matter of assembling the right plugins. I even [wrote one myself, to handle parsing Markdown within Yaml data](https://github.com/JonathanReeve/metalsmith-markdown-metadata). But in the end, I found the JavaScript and node.js world to be really fickle. My dozen Metalsmith plugins had hundreds of dependencies, in total, and updating one would often break another. I kept getting Dependabot security alerts on GitHub for dependencies I didn't even know I had. I *was* able to rewrite [my CV as YAML data](https://github.com/JonathanReeve/jonreeve.com/blob/36b7520fb9c71ffc09b6eec3007994b1c32c3e01/src/cv.yaml), though, and [format it using the satisfyingly minimalist templating language Pug](https://github.com/JonathanReeve/jonreeve.com/blob/36b7520fb9c71ffc09b6eec3007994b1c32c3e01/layouts/cv.pug). But I was still really in the same situation as before.

So this most recent time I thought hard about semantic data. YAML had allowed me to write my CV like this: 

```yaml
projects:
  - title: Corpus-DB
    url: https://github.com/JonathanReeve/corpus-db
    github: JonathanReeve/corpus-db
    start: 2017-03
    description: A database and API for plain text archives, for digital humanities research.
    updates:
      - date: 2017-10
        type: award
        description: Winner, [2017 NYCDH Graduate Student Project Award](https://nycdh.org/groups/nycdh-announcements-71439400/forum/topic/2017-nycdh-graduate-student-project-award-recipients/) 
---

This was a big step forward for my deduplication problem, since I could now write a template that could extract all `award` types and display those in a different section, without having to maintain those in two different places in the data. There is an *implicit* schema here: every project has a title, a url, and so on. But there was no way to keep this from breaking in an unexpected way, since I just had to remember that my template expects there to be certain fields in the YAML data. 

This is where Haskell comes in. Haskell allows me to define algebraic data types, like this: 

```haskell
data Project = Project {
  title :: Text,
  role :: ProjectRole,
  homepage :: URI,
  github :: Maybe Text,
  pypi :: Maybe Text,
  dateRange :: DateRange,
  desc :: Markdown,
  updates :: [ Update ]
  } deriving Show

data ProjectRole = Creator | CoCreator | Developer | Collaborator | ResearchAssistant deriving Show
```

What this says is: every project has a title, which is text, but the role I have in the project must come from a set list. Similarly, the URIs for the project must be URI types, which I can then validate and extract in predictable ways. This way, I can write an entry like this: 

```haskell

  Project { title = "Corpus-DB",
            role = Creator,
            homepage = "https://github.com/JonathanReeve/corpus-db",
            github = Just "JonathanReeve/corpus-db",
            dateRange = DateRange (date 2017 03) Present,
            desc  = "A database and API for plain text archives, for digital humanities research.",
            updates = [
              Update (date 2020 08) (Publication Abstract
                "Corpus-DB: a Scriptable Textual Corpus Database for Cultural Analytics"
                "https://dh2020.adho.org/wp-content/uploads/2020/07/604_CorpusDBaScriptableTextualCorpusDatabaseforCulturalAnalytics.html"
                (Venue "Digital Humanities 2020"
                       "https://dh2020.adho.org/"
                       "Ottawa, CA")),
```

At first glance, this may seem like an abuse of a programming language. Programming languages are for logic, the argument goes, and configuration belongs to configuration files, in formats like JSON, TOML, or YAML. We must separate logic and data, right? Actually, it turns out there are tons of advantages to writing data in Haskell. 

For one, it is compile-time type safe, meaning that my text editor actually checks that this compiles, as I'm writing it. If it were just plain text in YAML, there's nothing stopping a `date` from being something else entirely, like `foo`. But in Haskell, if I write a `date` that isn't two integers, (a year followed by a month), it'll raise an error immediately. 

Another advantage is composability. If I find myself repeating a given entry, I can just abstract it away into its own function. For instance, I have a `Venue` entry that looks like this: 

```haskell
cuEng = Venue "Department of English and Comparative Literature" "https://english.columbia.edu/" (uni "cu")
```

And whenever I want to use that, I can just call `venue = cuEng`, and save myself some typing. That also means that, should the name or URL of any of these entries change, I can change them in one place, without having to change them in seven. 

In addition to making everything type-safe, though, I greatly reduce the technologies involved. I'm still writing posts in Markdown, but have collapsed all the other templating languages to just Haskell. The HTML, CSS, and data, instead of being written in Liquid, Sass, YAML, and so forth, are all just in Haskell, which allows me to use the full power of the programming language to do whatever I need it to. For instance, if I'm doing something repeatedly, like adding a bunch of scripts: 

```html 
<script src="script1.js"/>
<script src="script2.js"/>
<script src="script3.js"/>
<script src="script4.js"/>
```

I can write that in Haskell like this: 

```haskell
script n = script_ [src_ "script" ++ n ++ ".js"]
map script [1..4]
```

And it's all validated at compile time, so when I press save in my text editor, it checks to make sure everything works. Because Haskell is a purely functional language, there are no runtime exceptions.

All of this I achieve using the wonderful static site generator [Rib](https://github.com/srid/rib), which built on the Haskell build tool [Shake](https://hackage.haskell.org/package/shake). So if there's anything else I need to do, which Rib doesn't provide out of the box, I don't need a plugin for it; I can just write a Shake action.

There's still some complexity left over, though, in that I still have to manage Haskell packages. But with [Nix](https://nixos.org/) this is trivial.
