package org.siemac.metamac.statistical.operations.core.dto;

import static org.junit.Assert.fail;

import org.junit.Test;
import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;

public class FamilyBaseDtoFieldsTest {

    @Test
    public void testExistsGetCode() {
        checkIfExistsMethodInFamilyBaseDto("getCode");
    }
    
    @Test
    public void testExistsGetUrn() {
        checkIfExistsMethodInFamilyBaseDto("getUrn");
    }
    
    @Test
    public void testExistsGetTitle() {
        checkIfExistsMethodInFamilyBaseDto("getTitle");
    }
    
    @Test
    public void testExistsGetAcronym() {
        checkIfExistsMethodInFamilyBaseDto("getAcronym");
    }
    
    @Test
    public void testExistsGetProcStatus() {
        checkIfExistsMethodInFamilyBaseDto("getProcStatus");
    }
    
    @Test
    public void testExistsGetDescription() {
        checkIfExistsMethodInFamilyBaseDto("getDescription");
    }
    
    @Test
    public void testExistsGetInternalInventoryDate() {
        checkIfExistsMethodInFamilyBaseDto("getInternalInventoryDate");
    }
    
    @Test
    public void testExistsGetCreatedDate() {
        checkIfExistsMethodInFamilyBaseDto("getCreatedDate");
    }
    
    private void checkIfExistsMethodInFamilyBaseDto(String methodName) {
        try {
            FamilyBaseDto.class.getMethod(methodName);
        } catch (NoSuchMethodException e) {
            fail("undefined method");
        }
    }

}
