## TODO

### Implement nested helpers removal:

````
{{#with foo=(max 4 (min 8 foo.bar))}}

{{firstname}} {{lastname}}

{{~/with}}
````

### Implement space removal:

````
{{#with person~}}

{{firstname}} {{lastname}}

{{~/with}}
````