
SELECT * FROM teste_ba.student;
SELECT * FROM teste_ba.subject;
SELECT * FROM teste_ba.assignments;

/*
tempo: 30 min
1- courses’ subject names and the number of students taking the course
only if the course has three or more students enrolled.
2- 
a. If there's no score, status = Pending
b. If score is 5.0 or more, status = Approved
c. If score is less than 5.0, status = Exam

*/

SELECT SB.SUBJECT_NAME
	,COUNT(*) AS CONTA
		#,COUNT(ST.STUDENT_NAME) AS NO_STUDENTS
FROM SUBJECT AS SB
JOIN STUDENT AS ST ON ST.SUBJECT_ID = SB.SUBJECT_ID
GROUP BY SB.SUBJECT_NAME
#HAVING COUNT(ST.STUDENT_NAME) > 3
HAVING COUNT(*) >= 3
;

SELECT SUBJECT_ID
	,COUNT(STUDENT_ID)
FROM STUDENT
GROUP BY 1;

SELECT ST.STUDENT_NAME
	,SB.SUBJECT_NAME
    ,IF(AI.SCORE >= 5.0 ,"APPROVED",IF(AI.SCORE < 5.0, "EXAM", "PENDING")) AS STATUS
    ,AI.SUBMISSION_DATE
FROM STUDENT AS ST
	LEFT JOIN SUBJECT AS SB ON SB.SUBJECT_ID = ST.SUBJECT_ID
    LEFT JOIN (
    SELECT STUDENT_ID
		,SUBJECT_ID
        ,SCORE
        ,MAX(SUBMISSION_DATE) AS SUBMISSION_DATE
        FROM ASSIGNMENTS
        GROUP BY 1, 2, 3
        ORDER BY 1, 2
    ) AS AI ON ST.STUDENT_ID = AI.STUDENT_ID
;
