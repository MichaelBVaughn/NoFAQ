DROP FUNCTION IF EXISTS upvoteRule
DELIMITER $$
CREATE PROCEDURE upvoteRule (IN rID INT)
BEGIN
	DECLARE oldVotes INT;
    START TRANSACTION;
    SET oldVotes = (SELECT votes FROM fixrule WHERE id = rID);
    UPDATE fixrule SET votes = oldVotes + 1 WHERE id = rID;
    COMMIT;
END	