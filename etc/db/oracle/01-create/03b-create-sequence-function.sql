-- Create GET_NEXT_SEQUENCE_VALUE function

CREATE OR REPLACE FUNCTION GET_NEXT_SEQUENCE_VALUE(sequence_name_in IN VARCHAR2)
  RETURN NUMBER
  IS sequence_next_value_out NUMBER(19,0);
  BEGIN
    SELECT SEQUENCE_NEXT_VALUE INTO sequence_next_value_out
    FROM TB_SEQUENCES 
    WHERE SEQUENCE_NAME = sequence_name_in;
    RETURN (sequence_next_value_out);
  END;
/