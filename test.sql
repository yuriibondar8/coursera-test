field1 as First_name
field3 as Last_name


SELECT
	* -- selects all attributes
FROM
	table1 -- comment
WHERE
	field1 = 'hello'; -- condition 1
	and field2 LIKE 'Ch%';  -- condition 2
	and field3 = 'world'; -- condition 3


SELECT
	COLUMN_NAME(S)
FROM
	TABLE_NAME
WHERE
	CONDITION
GROUP BY
	COLUMN_NAME(S)
HAVING
	AGGREGATE_CONDITION
ORDER BY
	COLUMN_NAME
LIMIT
	N