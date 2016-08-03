package org.siemac.metamac.statistical.operations.core.dto;

import static org.junit.Assert.fail;

import org.junit.Test;

public class InstanceBaseDtoFieldsTest {

    @Test
    public void testExistsGetCode() {
        checkIfExistsMethodInInstanceBaseDto("getCode");
    }
    
    @Test
    public void testExistsGetUrn() {
        checkIfExistsMethodInInstanceBaseDto("getUrn");
    }
    
    @Test
    public void testExistsGetTitle() {
        checkIfExistsMethodInInstanceBaseDto("getTitle");
    }
    
    @Test
    public void testExistsGetAcronym() {
        checkIfExistsMethodInInstanceBaseDto("getAcronym");
    }
    
    @Test
    public void testExistsGetProcStatus() {
        checkIfExistsMethodInInstanceBaseDto("getProcStatus");
    }
    
    @Test
    public void testExistsGetInstanceType() {
        checkIfExistsMethodInInstanceBaseDto("getInstanceType");
    }
    
    @Test
    public void testExistsGetDataDescription() {
        checkIfExistsMethodInInstanceBaseDto("getDataDescription");
    }
    
    @Test
    public void testExistsGetInternalInventoryDate() {
        checkIfExistsMethodInInstanceBaseDto("getInternalInventoryDate");
    }
    
    @Test
    public void testExistsGetCreatedDate() {
        checkIfExistsMethodInInstanceBaseDto("getCreatedDate");
    }
    
    
    private void checkIfExistsMethodInInstanceBaseDto(String methodName) {
        try {
            InstanceBaseDto.class.getMethod(methodName);
        } catch (NoSuchMethodException e) {
            fail("undefined method");
        }
    }

}
