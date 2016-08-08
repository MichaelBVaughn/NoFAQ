DROP PROCEDURE IF EXISTS getNumInvocationsWithNoFix;
DELIMITER $$
CREATE PROCEDURE getNumInvocationsWithNoFix(OUT numInv INT)
BEGIN
	SELECT COUNT(*) INTO numInv FROM invocation WHERE id NOT IN 
    (SELECT invocation_id from repairexample);
END	