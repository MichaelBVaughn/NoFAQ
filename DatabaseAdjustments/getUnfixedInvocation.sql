DROP PROCEDURE IF EXISTS getUnfixedInvocation;
DELIMITER $$
CREATE PROCEDURE getUnfixedInvocation (IN idx INT, OUT invCmd longtext, OUT invErr longtext)
BEGIN
	SELECT command.text, output.text INTO invCmd, invErr FROM invocation 
    INNER JOIN command ON command.id = invocation.cmd_id
    INNER JOIN output ON output.id = invocation.out_id
    WHERE invocation.id NOT IN 
		(SELECT invocation_id FROM repairexample) LIMIT idx, 1;
END	