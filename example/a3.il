Compiled Ruleset @602 
Compiled Rule 
	--atommatch:
		spec           [2, 2]
	--memmatch:
		spec           [1, 1]
		jump           [L135, [0], [], []]
	--guard:L135:
		spec           [1, 1]
		jump           [L128, [0], [], []]
	--body:L128:
		spec           [1, 4]
		commit         ["_init", 0]
		loadruleset    [0, @601]
		newatom      [1, 0, 'a'_0]
		newatom      [2, 0, 'a'_0]
		newatom      [3, 0, 'a'_0]
		proceed        []


Compiled Ruleset @601 
Compiled Rule 
	--atommatch:
		spec           [2, 2]
	--memmatch:
		spec           [1, 2]
		findatom     [1, 0, 'a'_0]
		commit         ["_a", 0]
		removeatom     [1, 0, 'a'_0]
		freeatom       [1]
		proceed        []


Inline
