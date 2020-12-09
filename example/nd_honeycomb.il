Compiled Ruleset @602 
Compiled Rule 
	--atommatch:
		spec           [2, 2]
	--memmatch:
		spec           [1, 1]
		jump           [L242, [0], [], []]
	--guard:L242:
		spec           [1, 1]
		jump           [L235, [0], [], []]
	--body:L235:
		spec           [1, 10]
		commit         ["_init", 0]
		loadruleset    [0, @601]
		newatom      [1, 0, 'r'_0]
		newatom      [2, 0, 'r'_0]
		newatom      [3, 0, 'r'_0]
		newatom      [4, 0, 'r'_0]
		newatom      [5, 0, 'l'_0]
		newatom      [6, 0, 'l'_0]
		newatom      [7, 0, 'l'_0]
		newatom      [8, 0, 'l'_0]
		newatom      [9, 0, 'b'_0]
		proceed        []


Compiled Ruleset @601    
Compiled Rule 
	--atommatch:
		spec           [2, 3]
	--memmatch:
		spec           [1, 4]
		findatom     [1, 0, 'b'_0]
		findatom     [2, 0, 'r'_0]
		commit         ["_brs", 0]
		removeatom     [1, 0, 'b'_0]
		removeatom     [2, 0, 'r'_0]
		newatom      [3, 0, 's'_0]
		freeatom       [1]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 3]
	--memmatch:
		spec           [1, 4]
		findatom     [1, 0, 'b'_0]
		findatom     [2, 0, 'l'_0]
		commit         ["_bls", 0]
		removeatom     [1, 0, 'b'_0]
		removeatom     [2, 0, 'l'_0]
		newatom      [3, 0, 's'_0]
		freeatom       [1]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 3]
	--memmatch:
		spec           [1, 5]
		findatom     [1, 0, 'r'_0]
		findatom     [2, 0, 's'_0]
		commit         ["_rsrb", 0]
		removeatom     [1, 0, 'r'_0]
		removeatom     [2, 0, 's'_0]
		newatom      [3, 0, 'r'_0]
		newatom      [4, 0, 'b'_0]
		freeatom       [1]
		freeatom       [2]
		proceed        []

Compiled Rule 
	--atommatch:
		spec           [2, 3]
	--memmatch:
		spec           [1, 5]
		findatom     [1, 0, 'l'_0]
		findatom     [2, 0, 's'_0]
		commit         ["_lslb", 0]
		removeatom     [1, 0, 'l'_0]
		removeatom     [2, 0, 's'_0]
		newatom      [3, 0, 'l'_0]
		newatom      [4, 0, 'b'_0]
		freeatom       [1]
		freeatom       [2]
		proceed        []


Inline
