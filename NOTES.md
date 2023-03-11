Everything is mostly based on helper: 

- The `{{> partial foo bar name=blabla }}` is nothing more than a helper that calls the partial with that parameters
- The `{{ foo }}` it is an alias for `{{lookup . "foo"}}` also `{{helper a}}` it is an alias for `{{ helper (lookup . "a") }}`
- The `{{ foo.bar }}` it is an alias for `{{lookup (lookup . "foo") "bar"}}` also `{{helper a}}` it is an alias for `{{ helper (lookup . "a") }}`
- The `{{#block a b c}}` is the same as a helper, where the body of the block (the list of children) is passed as a parameter. And also the list of else
- The `{{#  block a b c}}` also is valid as well as `{{/  block }}`

- The `{{ 'foo' }}` or `{{ "foo" }}` are the string `foo`. 
- 
- The doc says: "JavaScript-style strings, " and ', may also be used instead of [ pairs." but it is only true if there is only one segment. While the `[]` are supported always. Also `..` and `.` seems supported only at the beginning however, I don't see why. 

So in handlebar.js:
`{{ 'foo' }} = {{ lookup . "foo"}}`
`{{ 'foo.bar' }} = {{ lookup . "foo.bar"}}`
`{{ "foo.bar" }} = {{ lookup . "foo.bar"}}`
`{{ foo.'bar' }} = error`
`{{ foo.[bar] }} = {{ lookup (lookup . "foo") "bar"}}`
`{{ 'loud' foo }} = {{ loud (lookup . "foo")}}`

In this version
`{{ 'foo' }} = {{ render "foo"}}`
`{{ 'foo.bar' }} = {{ render "foo.bar"}}`
`{{ "foo.bar" }} = {{ render "foo.bar"}}`
`{{ foo.'bar' }} = error`
`{{ 'loud' foo }} = error` to invoke a helper with weird name `{{ helper 'loud' foo}}`


- This should be supported: `{{ foo.[bar].[2].baz/.././baz }}`. eg `{{ foo/../foo }} == {{ foo }}`



Also, the function can return any type of value, so the `{{}}` or `{{{}}}` should apply a `render` function so `{{ foo }}` should be seen as `render(lookup(".", "foo"))`

https://handlebarsjs.com/playground.html#format=1&currentExample=%7B%22template%22%3A%22%7B%7Bfirstname%7D%7D%20%7B%7Bloud%20(lookup%20.%20%5C%22lastname%5C%22)%7D%7D%20%7B%7B%20%5C%22lastname%5C%22%7D%7D%20%7B%7B%20lookup%20.%20%5C%22lastname%5C%22%7D%7D%20%7B%7Becho%205%7D%7D%20%7B%7Bplus%205%206%7D%7D%5Cn%20%7B%7B%23%20if%20black%7D%7Dit%20is%20black%7B%7B%2Fif%7D%7D%5Cn%22%2C%22partials%22%3A%5B%5D%2C%22input%22%3A%22%7B%5Cn%20%20firstname%3A%20%5C%22Yehuda%5C%22%2C%5Cn%20%20lastname%3A%20%5C%22Katz%5C%22%2C%5Cn%20%20black%3A%20false%5Cn%7D%5Cn%22%2C%22output%22%3A%22Yehuda%20KATZ%20Katz%20Katz%205%2011%5Cn%20%5Cn%22%2C%22preparationScript%22%3A%22Handlebars.registerHelper('loud'%2C%20function%20(aString)%20%7B%5Cn%20%20%20%20return%20aString.toUpperCase()%5Cn%7D)%5Cn%5CnHandlebars.registerHelper('echo'%2C%20function%20(aString)%20%7B%5Cn%20%20%20%20return%20aString%5Cn%7D)%5Cn%5CnHandlebars.registerHelper('plus'%2C%20function%20(a%2C%20b)%20%7B%5Cn%20%20%20%20return%20a%20%2B%20b%5Cn%7D)%5Cn%5Cn%22%2C%22handlebarsVersion%22%3A%224.7.7%22%7D