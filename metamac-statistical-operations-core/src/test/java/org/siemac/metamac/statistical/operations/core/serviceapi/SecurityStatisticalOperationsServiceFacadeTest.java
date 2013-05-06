package org.siemac.metamac.statistical.operations.core.serviceapi;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang.RandomStringUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.siemac.metamac.core.common.criteria.MetamacCriteria;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.core.common.dto.LocalisedStringDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.core.utils.StatisticalOperationsBaseTest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.transaction.TransactionConfiguration;
import org.springframework.transaction.annotation.Transactional;

/**
 * Spring based transactional test with DbUnit support.
 */

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:spring/statistical-operations/applicationContext-test.xml"})
@TransactionConfiguration(transactionManager = "txManager", defaultRollback = true)
@Transactional
@DirtiesContext(classMode = ClassMode.AFTER_CLASS)
public class SecurityStatisticalOperationsServiceFacadeTest extends StatisticalOperationsBaseTest implements StatisticalOperationsServiceFacadeTestBase {

    @Autowired
    protected StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    @Test
    public void testFindAllSurveyTypes() throws Exception {
        statisticalOperationsServiceFacade.findAllSurveyTypes(getServiceContextTecnicoApoyoDifusion());
        statisticalOperationsServiceFacade.findAllSurveyTypes(getServiceContextTecnicoApoyoPlanificacion());
        statisticalOperationsServiceFacade.findAllSurveyTypes(getServiceContextTecnicoApoyoProduccion());
        statisticalOperationsServiceFacade.findAllSurveyTypes(getServiceContextTecnicoDifusion());
        statisticalOperationsServiceFacade.findAllSurveyTypes(getServiceContextTecnicoPlanificacion());
        statisticalOperationsServiceFacade.findAllSurveyTypes(getServiceContextTecnicoProduccion());
    }

    @Test
    public void testFindSurveyTypeById() throws Exception {
        statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextTecnicoApoyoDifusion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextTecnicoApoyoPlanificacion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextTecnicoApoyoProduccion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextTecnicoDifusion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextTecnicoPlanificacion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextTecnicoProduccion(), Long.valueOf(1));
    }

    @Test
    public void testFindAllInstanceTypes() throws Exception {
        statisticalOperationsServiceFacade.findAllInstanceTypes(getServiceContextTecnicoApoyoDifusion());
        statisticalOperationsServiceFacade.findAllInstanceTypes(getServiceContextTecnicoApoyoPlanificacion());
        statisticalOperationsServiceFacade.findAllInstanceTypes(getServiceContextTecnicoApoyoProduccion());
        statisticalOperationsServiceFacade.findAllInstanceTypes(getServiceContextTecnicoDifusion());
        statisticalOperationsServiceFacade.findAllInstanceTypes(getServiceContextTecnicoPlanificacion());
        statisticalOperationsServiceFacade.findAllInstanceTypes(getServiceContextTecnicoProduccion());
    }

    @Test
    public void testFindInstanceTypeById() throws Exception {
        statisticalOperationsServiceFacade.findInstanceTypeById(getServiceContextTecnicoApoyoDifusion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findInstanceTypeById(getServiceContextTecnicoApoyoPlanificacion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findInstanceTypeById(getServiceContextTecnicoApoyoProduccion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findInstanceTypeById(getServiceContextTecnicoDifusion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findInstanceTypeById(getServiceContextTecnicoPlanificacion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findInstanceTypeById(getServiceContextTecnicoProduccion(), Long.valueOf(1));
    }

    @Test
    public void testFindAllSurveySources() throws Exception {
        statisticalOperationsServiceFacade.findAllSurveySources(getServiceContextTecnicoApoyoDifusion());
        statisticalOperationsServiceFacade.findAllSurveySources(getServiceContextTecnicoApoyoPlanificacion());
        statisticalOperationsServiceFacade.findAllSurveySources(getServiceContextTecnicoApoyoProduccion());
        statisticalOperationsServiceFacade.findAllSurveySources(getServiceContextTecnicoDifusion());
        statisticalOperationsServiceFacade.findAllSurveySources(getServiceContextTecnicoPlanificacion());
        statisticalOperationsServiceFacade.findAllSurveySources(getServiceContextTecnicoProduccion());
    }

    @Test
    public void testFindSurveySourceById() throws Exception {
        statisticalOperationsServiceFacade.findSurveySourceById(getServiceContextTecnicoApoyoDifusion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findSurveySourceById(getServiceContextTecnicoApoyoPlanificacion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findSurveySourceById(getServiceContextTecnicoApoyoProduccion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findSurveySourceById(getServiceContextTecnicoDifusion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findSurveySourceById(getServiceContextTecnicoPlanificacion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findSurveySourceById(getServiceContextTecnicoProduccion(), Long.valueOf(1));

    }

    @Test
    public void testFindAllOfficialityTypes() throws Exception {
        statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextTecnicoApoyoDifusion());
        statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextTecnicoApoyoPlanificacion());
        statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextTecnicoApoyoProduccion());
        statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextTecnicoDifusion());
        statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextTecnicoPlanificacion());
        statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextTecnicoProduccion());

    }

    @Test
    public void testFindOfficialityTypeById() throws Exception {
        statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContextTecnicoApoyoDifusion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContextTecnicoApoyoPlanificacion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContextTecnicoApoyoProduccion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContextTecnicoDifusion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContextTecnicoPlanificacion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContextTecnicoProduccion(), Long.valueOf(1));

    }

    @Test
    public void testFindAllCollMethods() throws Exception {
        statisticalOperationsServiceFacade.findAllCollMethods(getServiceContextTecnicoApoyoDifusion());
        statisticalOperationsServiceFacade.findAllCollMethods(getServiceContextTecnicoApoyoPlanificacion());
        statisticalOperationsServiceFacade.findAllCollMethods(getServiceContextTecnicoApoyoProduccion());
        statisticalOperationsServiceFacade.findAllCollMethods(getServiceContextTecnicoDifusion());
        statisticalOperationsServiceFacade.findAllCollMethods(getServiceContextTecnicoPlanificacion());
        statisticalOperationsServiceFacade.findAllCollMethods(getServiceContextTecnicoProduccion());
    }

    @Test
    public void testFindCollMethodById() throws Exception {
        statisticalOperationsServiceFacade.findCollMethodById(getServiceContextTecnicoApoyoDifusion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findCollMethodById(getServiceContextTecnicoApoyoPlanificacion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findCollMethodById(getServiceContextTecnicoApoyoProduccion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findCollMethodById(getServiceContextTecnicoDifusion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findCollMethodById(getServiceContextTecnicoPlanificacion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findCollMethodById(getServiceContextTecnicoProduccion(), Long.valueOf(1));
    }

    @Test
    public void testFindAllCosts() throws Exception {
        statisticalOperationsServiceFacade.findAllCosts(getServiceContextTecnicoApoyoDifusion());
        statisticalOperationsServiceFacade.findAllCosts(getServiceContextTecnicoApoyoPlanificacion());
        statisticalOperationsServiceFacade.findAllCosts(getServiceContextTecnicoApoyoProduccion());
        statisticalOperationsServiceFacade.findAllCosts(getServiceContextTecnicoDifusion());
        statisticalOperationsServiceFacade.findAllCosts(getServiceContextTecnicoPlanificacion());
        statisticalOperationsServiceFacade.findAllCosts(getServiceContextTecnicoProduccion());

    }

    @Test
    public void testFindCostById() throws Exception {
        statisticalOperationsServiceFacade.findCostById(getServiceContextTecnicoApoyoDifusion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findCostById(getServiceContextTecnicoApoyoPlanificacion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findCostById(getServiceContextTecnicoApoyoProduccion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findCostById(getServiceContextTecnicoDifusion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findCostById(getServiceContextTecnicoPlanificacion(), Long.valueOf(1));
        statisticalOperationsServiceFacade.findCostById(getServiceContextTecnicoProduccion(), Long.valueOf(1));

    }

    @Test
    public void testCreateFamily() throws Exception {
        FamilyDto familyDto = createFamilyDto();

        statisticalOperationsServiceFacade.createFamily(getServiceContextTecnicoPlanificacion(), familyDto);
        try {
            statisticalOperationsServiceFacade.createFamily(getServiceContextTecnicoApoyoDifusion(), familyDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.createFamily(getServiceContextTecnicoApoyoPlanificacion(), familyDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.createFamily(getServiceContextTecnicoApoyoProduccion(), familyDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.createFamily(getServiceContextTecnicoDifusion(), familyDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.createFamily(getServiceContextTecnicoProduccion(), familyDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testUpdateFamily() throws Exception {
        FamilyDto familyDto = createFamilyDto();
        familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), familyDto);
        familyDto.setCode("FAMILY-MODIFIED-" + RandomStringUtils.random(50, true, true));

        statisticalOperationsServiceFacade.updateFamily(getServiceContextTecnicoPlanificacion(), familyDto);
        try {
            statisticalOperationsServiceFacade.updateFamily(getServiceContextTecnicoApoyoDifusion(), familyDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.updateFamily(getServiceContextTecnicoApoyoPlanificacion(), familyDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.updateFamily(getServiceContextTecnicoApoyoProduccion(), familyDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.updateFamily(getServiceContextTecnicoDifusion(), familyDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.updateFamily(getServiceContextTecnicoProduccion(), familyDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testDeleteFamily() throws Exception {
        FamilyDto familyDto = createFamilyDto();
        Long familyId = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), familyDto).getId();

        statisticalOperationsServiceFacade.deleteFamily(getServiceContextTecnicoPlanificacion(), familyId);
        try {
            statisticalOperationsServiceFacade.deleteFamily(getServiceContextTecnicoApoyoDifusion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.deleteFamily(getServiceContextTecnicoApoyoPlanificacion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.deleteFamily(getServiceContextTecnicoApoyoProduccion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.deleteFamily(getServiceContextTecnicoDifusion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.deleteFamily(getServiceContextTecnicoProduccion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testFindAllFamilies() throws Exception {
        statisticalOperationsServiceFacade.findAllFamilies(getServiceContextTecnicoApoyoDifusion());
        statisticalOperationsServiceFacade.findAllFamilies(getServiceContextTecnicoApoyoPlanificacion());
        statisticalOperationsServiceFacade.findAllFamilies(getServiceContextTecnicoApoyoProduccion());
        statisticalOperationsServiceFacade.findAllFamilies(getServiceContextTecnicoDifusion());
        statisticalOperationsServiceFacade.findAllFamilies(getServiceContextTecnicoPlanificacion());
        statisticalOperationsServiceFacade.findAllFamilies(getServiceContextTecnicoProduccion());
    }

    @Test
    public void testFindFamilyByCondition() throws Exception {
        MetamacCriteria criteria = new MetamacCriteria();

        statisticalOperationsServiceFacade.findFamilyByCondition(getServiceContextTecnicoApoyoDifusion(), criteria);
        statisticalOperationsServiceFacade.findFamilyByCondition(getServiceContextTecnicoApoyoPlanificacion(), criteria);
        statisticalOperationsServiceFacade.findFamilyByCondition(getServiceContextTecnicoApoyoProduccion(), criteria);
        statisticalOperationsServiceFacade.findFamilyByCondition(getServiceContextTecnicoDifusion(), criteria);
        statisticalOperationsServiceFacade.findFamilyByCondition(getServiceContextTecnicoPlanificacion(), criteria);
        statisticalOperationsServiceFacade.findFamilyByCondition(getServiceContextTecnicoProduccion(), criteria);
    }

    @Test
    public void testFindFamilyById() throws Exception {
        Long familyId = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto()).getId();

        statisticalOperationsServiceFacade.findFamilyById(getServiceContextTecnicoApoyoDifusion(), familyId);
        statisticalOperationsServiceFacade.findFamilyById(getServiceContextTecnicoApoyoPlanificacion(), familyId);
        statisticalOperationsServiceFacade.findFamilyById(getServiceContextTecnicoApoyoProduccion(), familyId);
        statisticalOperationsServiceFacade.findFamilyById(getServiceContextTecnicoDifusion(), familyId);
        statisticalOperationsServiceFacade.findFamilyById(getServiceContextTecnicoPlanificacion(), familyId);
        statisticalOperationsServiceFacade.findFamilyById(getServiceContextTecnicoProduccion(), familyId);

    }

    @Test
    public void testFindFamilyByCode() throws Exception {
        String familyCode = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto()).getCode();

        statisticalOperationsServiceFacade.findFamilyByCode(getServiceContextTecnicoApoyoDifusion(), familyCode);
        statisticalOperationsServiceFacade.findFamilyByCode(getServiceContextTecnicoApoyoPlanificacion(), familyCode);
        statisticalOperationsServiceFacade.findFamilyByCode(getServiceContextTecnicoApoyoProduccion(), familyCode);
        statisticalOperationsServiceFacade.findFamilyByCode(getServiceContextTecnicoDifusion(), familyCode);
        statisticalOperationsServiceFacade.findFamilyByCode(getServiceContextTecnicoPlanificacion(), familyCode);
        statisticalOperationsServiceFacade.findFamilyByCode(getServiceContextTecnicoProduccion(), familyCode);
    }

    @Test
    public void testFindFamilyByUrn() throws Exception {
        String familyUrn = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto()).getUrn();

        statisticalOperationsServiceFacade.findFamilyByUrn(getServiceContextTecnicoApoyoDifusion(), familyUrn);
        statisticalOperationsServiceFacade.findFamilyByUrn(getServiceContextTecnicoApoyoPlanificacion(), familyUrn);
        statisticalOperationsServiceFacade.findFamilyByUrn(getServiceContextTecnicoApoyoProduccion(), familyUrn);
        statisticalOperationsServiceFacade.findFamilyByUrn(getServiceContextTecnicoDifusion(), familyUrn);
        statisticalOperationsServiceFacade.findFamilyByUrn(getServiceContextTecnicoPlanificacion(), familyUrn);
        statisticalOperationsServiceFacade.findFamilyByUrn(getServiceContextTecnicoProduccion(), familyUrn);

    }

    @Test
    public void testPublishInternallyFamily() throws Exception {
        FamilyDto familyDto = createFamilyDto();
        Long familyId = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), familyDto).getId();
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyId, operationId);

        statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextTecnicoPlanificacion(), familyId);
        try {
            statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextTecnicoApoyoDifusion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextTecnicoApoyoPlanificacion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextTecnicoApoyoProduccion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextTecnicoDifusion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextTecnicoProduccion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testPublishExternallyFamily() throws Exception {
        FamilyDto familyDto = createFamilyDto();
        Long familyId = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), familyDto).getId();
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);
        statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationId);
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyId, operationId);
        statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextAdministrador(), familyId);

        statisticalOperationsServiceFacade.publishExternallyFamily(getServiceContextTecnicoPlanificacion(), familyId);
        try {
            statisticalOperationsServiceFacade.publishExternallyFamily(getServiceContextTecnicoApoyoDifusion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.publishExternallyFamily(getServiceContextTecnicoApoyoPlanificacion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.publishExternallyFamily(getServiceContextTecnicoApoyoProduccion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.publishExternallyFamily(getServiceContextTecnicoDifusion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.publishExternallyFamily(getServiceContextTecnicoProduccion(), familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testFindOperationsForFamily() throws Exception {
        Long familyId = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto()).getId();

        statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContextTecnicoApoyoDifusion(), familyId);
        statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContextTecnicoApoyoPlanificacion(), familyId);
        statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContextTecnicoApoyoProduccion(), familyId);
        statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContextTecnicoDifusion(), familyId);
        statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContextTecnicoPlanificacion(), familyId);
        statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContextTecnicoProduccion(), familyId);
    }

    @Test
    public void testAddOperationForFamily() throws Exception {
        FamilyDto familyDto = createFamilyDto();
        Long familyId = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), familyDto).getId();
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();

        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextTecnicoPlanificacion(), familyId, operationId);
        try {
            statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextTecnicoApoyoDifusion(), familyId, operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextTecnicoApoyoPlanificacion(), familyId, operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextTecnicoApoyoProduccion(), familyId, operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextTecnicoDifusion(), familyId, operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextTecnicoProduccion(), familyId, operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testRemoveOperationForFamily() throws Exception {
        FamilyDto familyDto = createFamilyDto();
        Long familyId = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), familyDto).getId();
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyId, operationId);

        statisticalOperationsServiceFacade.removeOperationForFamily(getServiceContextTecnicoPlanificacion(), familyId, operationId);
        try {
            statisticalOperationsServiceFacade.removeOperationForFamily(getServiceContextTecnicoApoyoDifusion(), familyId, operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.removeOperationForFamily(getServiceContextTecnicoApoyoPlanificacion(), familyId, operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.removeOperationForFamily(getServiceContextTecnicoApoyoProduccion(), familyId, operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.removeOperationForFamily(getServiceContextTecnicoDifusion(), familyId, operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.removeOperationForFamily(getServiceContextTecnicoProduccion(), familyId, operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testCreateOperation() throws Exception {
        OperationDto operationDto = createOperationDto();

        statisticalOperationsServiceFacade.createOperation(getServiceContextTecnicoPlanificacion(), operationDto);
        try {
            statisticalOperationsServiceFacade.createOperation(getServiceContextTecnicoApoyoDifusion(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.createOperation(getServiceContextTecnicoApoyoPlanificacion(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.createOperation(getServiceContextTecnicoApoyoProduccion(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.createOperation(getServiceContextTecnicoDifusion(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.createOperation(getServiceContextTecnicoProduccion(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testUpdateOperation() throws Exception {
        OperationDto operationDto = createOperationDto();
        operationDto.setCode("C0025A");
        operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto);

        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextTecnicoPlanificacion(), operationDto);
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextTecnicoProduccionOperation01(), operationDto);
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextTecnicoApoyoPlanificacion(), operationDto);
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextTecnicoProduccion(), operationDto);

        try {
            statisticalOperationsServiceFacade.updateOperation(getServiceContextTecnicoApoyoDifusion(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.updateOperation(getServiceContextTecnicoApoyoProduccion(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.updateOperation(getServiceContextTecnicoDifusion(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.updateOperation(getServiceContextTecnicoProduccionOperation02(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_ACCESS_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testDeleteOperation() throws Exception {
        OperationDto operationDto = createOperationDto();
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();

        statisticalOperationsServiceFacade.deleteOperation(getServiceContextTecnicoPlanificacion(), operationId);
        try {
            statisticalOperationsServiceFacade.deleteOperation(getServiceContextTecnicoApoyoDifusion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.deleteOperation(getServiceContextTecnicoApoyoPlanificacion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.deleteOperation(getServiceContextTecnicoApoyoProduccion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.deleteOperation(getServiceContextTecnicoDifusion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.deleteOperation(getServiceContextTecnicoProduccion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testFindAllOperations() throws Exception {
        statisticalOperationsServiceFacade.findAllOperations(getServiceContextTecnicoApoyoDifusion());
        statisticalOperationsServiceFacade.findAllOperations(getServiceContextTecnicoApoyoPlanificacion());
        statisticalOperationsServiceFacade.findAllOperations(getServiceContextTecnicoApoyoProduccion());
        statisticalOperationsServiceFacade.findAllOperations(getServiceContextTecnicoDifusion());
        statisticalOperationsServiceFacade.findAllOperations(getServiceContextTecnicoPlanificacion());
        statisticalOperationsServiceFacade.findAllOperations(getServiceContextTecnicoProduccion());
    }

    @Test
    public void testFindOperationsByCondition() throws Exception {
        MetamacCriteria criteria = new MetamacCriteria();

        statisticalOperationsServiceFacade.findOperationsByCondition(getServiceContextTecnicoApoyoDifusion(), criteria);
        statisticalOperationsServiceFacade.findOperationsByCondition(getServiceContextTecnicoApoyoPlanificacion(), criteria);
        statisticalOperationsServiceFacade.findOperationsByCondition(getServiceContextTecnicoApoyoProduccion(), criteria);
        statisticalOperationsServiceFacade.findOperationsByCondition(getServiceContextTecnicoDifusion(), criteria);
        statisticalOperationsServiceFacade.findOperationsByCondition(getServiceContextTecnicoPlanificacion(), criteria);
        statisticalOperationsServiceFacade.findOperationsByCondition(getServiceContextTecnicoProduccion(), criteria);
    }

    @Test
    public void testFindOperationById() throws Exception {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto()).getId();

        statisticalOperationsServiceFacade.findOperationById(getServiceContextTecnicoApoyoDifusion(), operationId);
        statisticalOperationsServiceFacade.findOperationById(getServiceContextTecnicoApoyoPlanificacion(), operationId);
        statisticalOperationsServiceFacade.findOperationById(getServiceContextTecnicoApoyoProduccion(), operationId);
        statisticalOperationsServiceFacade.findOperationById(getServiceContextTecnicoDifusion(), operationId);
        statisticalOperationsServiceFacade.findOperationById(getServiceContextTecnicoPlanificacion(), operationId);
        statisticalOperationsServiceFacade.findOperationById(getServiceContextTecnicoProduccion(), operationId);
    }

    @Test
    public void testFindOperationByCode() throws Exception {
        String operationCode = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto()).getCode();

        statisticalOperationsServiceFacade.findOperationByCode(getServiceContextTecnicoApoyoDifusion(), operationCode);
        statisticalOperationsServiceFacade.findOperationByCode(getServiceContextTecnicoApoyoPlanificacion(), operationCode);
        statisticalOperationsServiceFacade.findOperationByCode(getServiceContextTecnicoApoyoProduccion(), operationCode);
        statisticalOperationsServiceFacade.findOperationByCode(getServiceContextTecnicoDifusion(), operationCode);
        statisticalOperationsServiceFacade.findOperationByCode(getServiceContextTecnicoPlanificacion(), operationCode);
        statisticalOperationsServiceFacade.findOperationByCode(getServiceContextTecnicoProduccion(), operationCode);
    }

    @Test
    public void testFindOperationByUrn() throws Exception {
        String operationUrn = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto()).getUrn();

        statisticalOperationsServiceFacade.findOperationByUrn(getServiceContextTecnicoApoyoDifusion(), operationUrn);
        statisticalOperationsServiceFacade.findOperationByUrn(getServiceContextTecnicoApoyoPlanificacion(), operationUrn);
        statisticalOperationsServiceFacade.findOperationByUrn(getServiceContextTecnicoApoyoProduccion(), operationUrn);
        statisticalOperationsServiceFacade.findOperationByUrn(getServiceContextTecnicoDifusion(), operationUrn);
        statisticalOperationsServiceFacade.findOperationByUrn(getServiceContextTecnicoPlanificacion(), operationUrn);
        statisticalOperationsServiceFacade.findOperationByUrn(getServiceContextTecnicoProduccion(), operationUrn);
    }

    @Test
    public void testPublishInternallyOperation() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();

        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

    }

    @Test
    public void testPublishInternallyOperationPlanificacion() throws Exception {

        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();

        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextTecnicoPlanificacion(), operationId);
    }

    @Test
    public void testPublishInternallyOperationOperation01() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        try {
            statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextTecnicoProduccionOperation01(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishInternallyOperationTecnicoProduccion() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();

        try {
            statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextTecnicoProduccion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishInternallyOperationTecnicoApoyoDifusion() throws Exception {

        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        try {
            statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextTecnicoApoyoDifusion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishInternallyOperationTecnicoApoyoProduccion() throws Exception {

        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        try {
            statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextTecnicoApoyoProduccion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishInternallyOperationTecnicoDifusion() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        try {
            statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextTecnicoDifusion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishInternallyOperationTecnicoProduccionOperation02() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        try {
            statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextTecnicoProduccionOperation02(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishInternallyOperationTecnicoApoyoPlanificacion() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        try {
            statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextTecnicoApoyoPlanificacion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishExternallyOperation() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationId);

    }

    @Test
    public void testPublishExternallyOperationTecnicoPlanificacion() throws Exception {

        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextTecnicoPlanificacion(), operationId);
    }

    @Test
    public void testPublishExternallyOperationOperation01() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        try {
            statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextTecnicoProduccionOperation01(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishExternallyOperationTecnicoProduccion() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        try {
            statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextTecnicoProduccion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishExternallyOperationTecnicoApoyoDifusion() throws Exception {

        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        try {
            statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextTecnicoApoyoDifusion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishExternallyOperationTecnicoApoyoProduccion() throws Exception {

        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        try {
            statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextTecnicoApoyoProduccion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishExternallyOperationTecnicoDifusion() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        try {
            statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextTecnicoDifusion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishExternallyOperationTecnicoProduccionOperation02() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        try {
            statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextTecnicoProduccionOperation02(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testPublishExternallyOperationTecnicoApoyoPlanificacion() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        try {
            statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextTecnicoApoyoPlanificacion(), operationId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testFindFamiliesForOperation() throws Exception {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto()).getId();

        statisticalOperationsServiceFacade.findFamiliesForOperation(getServiceContextTecnicoApoyoDifusion(), operationId);
        statisticalOperationsServiceFacade.findFamiliesForOperation(getServiceContextTecnicoApoyoPlanificacion(), operationId);
        statisticalOperationsServiceFacade.findFamiliesForOperation(getServiceContextTecnicoApoyoProduccion(), operationId);
        statisticalOperationsServiceFacade.findFamiliesForOperation(getServiceContextTecnicoDifusion(), operationId);
        statisticalOperationsServiceFacade.findFamiliesForOperation(getServiceContextTecnicoPlanificacion(), operationId);
        statisticalOperationsServiceFacade.findFamiliesForOperation(getServiceContextTecnicoProduccion(), operationId);
    }

    @Test
    public void testFindInstancesForOperation() throws Exception {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto()).getId();

        statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextTecnicoApoyoDifusion(), operationId);
        statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextTecnicoApoyoPlanificacion(), operationId);
        statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextTecnicoApoyoProduccion(), operationId);
        statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextTecnicoDifusion(), operationId);
        statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextTecnicoPlanificacion(), operationId);
        statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextTecnicoProduccion(), operationId);
    }

    @Test
    public void testAddFamilyForOperation() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        Long familyId = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto()).getId();

        statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextTecnicoPlanificacion(), operationId, familyId);
        statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextTecnicoProduccionOperation01(), operationId, familyId);
        statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextTecnicoApoyoPlanificacion(), operationId, familyId);
        statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextTecnicoProduccion(), operationId, familyId);

        try {
            statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextTecnicoApoyoDifusion(), operationId, familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextTecnicoApoyoProduccion(), operationId, familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextTecnicoDifusion(), operationId, familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextTecnicoProduccionOperation02(), operationId, familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_ACCESS_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testRemoveFamilyForOperation() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        Long familyId = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto()).getId();

        statisticalOperationsServiceFacade.removeFamilyForOperation(getServiceContextTecnicoPlanificacion(), operationId, familyId);
        statisticalOperationsServiceFacade.removeFamilyForOperation(getServiceContextTecnicoProduccionOperation01(), operationId, familyId);
        statisticalOperationsServiceFacade.removeFamilyForOperation(getServiceContextTecnicoApoyoPlanificacion(), operationId, familyId);
        statisticalOperationsServiceFacade.removeFamilyForOperation(getServiceContextTecnicoProduccion(), operationId, familyId);

        try {
            statisticalOperationsServiceFacade.removeFamilyForOperation(getServiceContextTecnicoApoyoDifusion(), operationId, familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.removeFamilyForOperation(getServiceContextTecnicoApoyoProduccion(), operationId, familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.removeFamilyForOperation(getServiceContextTecnicoDifusion(), operationId, familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.removeFamilyForOperation(getServiceContextTecnicoProduccionOperation02(), operationId, familyId);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_ACCESS_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testCreateInstance() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        statisticalOperationsServiceFacade.createInstance(getServiceContextTecnicoPlanificacion(), operationId, createInstanceDto());
        statisticalOperationsServiceFacade.createInstance(getServiceContextTecnicoApoyoPlanificacion(), operationId, createInstanceDto());
        statisticalOperationsServiceFacade.createInstance(getServiceContextTecnicoApoyoProduccion(), operationId, createInstanceDto());
        statisticalOperationsServiceFacade.createInstance(getServiceContextTecnicoProduccion(), operationId, createInstanceDto());
        statisticalOperationsServiceFacade.createInstance(getServiceContextTecnicoProduccionOperation01(), operationId, createInstanceDto());

        try {
            statisticalOperationsServiceFacade.createInstance(getServiceContextTecnicoApoyoDifusion(), operationId, createInstanceDto());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.createInstance(getServiceContextTecnicoDifusion(), operationId, createInstanceDto());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.createInstance(getServiceContextTecnicoProduccionOperation02(), operationId, createInstanceDto());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_ACCESS_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testUpdateInstance() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto());

        instanceDto = statisticalOperationsServiceFacade.updateInstance(getServiceContextTecnicoPlanificacion(), instanceDto);
        instanceDto = statisticalOperationsServiceFacade.updateInstance(getServiceContextTecnicoApoyoPlanificacion(), instanceDto);
        instanceDto = statisticalOperationsServiceFacade.updateInstance(getServiceContextTecnicoApoyoProduccion(), instanceDto);
        instanceDto = statisticalOperationsServiceFacade.updateInstance(getServiceContextTecnicoProduccion(), instanceDto);
        instanceDto = statisticalOperationsServiceFacade.updateInstance(getServiceContextTecnicoProduccionOperation01(), instanceDto);

        try {
            statisticalOperationsServiceFacade.updateInstance(getServiceContextTecnicoApoyoDifusion(), instanceDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.updateInstance(getServiceContextTecnicoDifusion(), instanceDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.updateInstance(getServiceContextTecnicoProduccionOperation02(), instanceDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_ACCESS_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testUpdateInstancesOrder() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto());
        List<Long> order = new ArrayList<Long>();
        order.add(Long.valueOf(instanceDto.getId()));

        statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContextAdministrador(), operationId, order);
        statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContextTecnicoApoyoPlanificacion(), operationId, order);
        statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContextTecnicoApoyoProduccion(), operationId, order);
        statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContextTecnicoProduccion(), operationId, order);
        statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContextTecnicoProduccionOperation01(), operationId, order);

        try {
            statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContextTecnicoApoyoDifusion(), operationId, order);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContextTecnicoDifusion(), operationId, order);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }
        try {
            statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContextTecnicoProduccionOperation02(), operationId, order);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.SECURITY_ACCESS_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testDeleteInstance() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        // getServiceContextTecnicoPlanificacion
        {
            Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();
            statisticalOperationsServiceFacade.deleteInstance(getServiceContextTecnicoPlanificacion(), instanceId);
        }

        // getServiceContextTecnicoProduccion
        {
            Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();
            statisticalOperationsServiceFacade.deleteInstance(getServiceContextTecnicoProduccion(), instanceId);
        }

        // getServiceContextTecnicoProduccionOperation01
        {
            Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();
            statisticalOperationsServiceFacade.deleteInstance(getServiceContextTecnicoProduccionOperation01(), instanceId);
        }

        // Not allowed roles
        {
            Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();

            try {
                statisticalOperationsServiceFacade.deleteInstance(getServiceContextTecnicoApoyoPlanificacion(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }

            try {
                statisticalOperationsServiceFacade.deleteInstance(getServiceContextTecnicoApoyoProduccion(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }

            try {
                statisticalOperationsServiceFacade.deleteInstance(getServiceContextTecnicoApoyoDifusion(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }
            try {
                statisticalOperationsServiceFacade.deleteInstance(getServiceContextTecnicoDifusion(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }
            try {
                statisticalOperationsServiceFacade.deleteInstance(getServiceContextTecnicoProduccionOperation02(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_ACCESS_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }
        }
    }

    @Test
    public void testFindAllInstances() throws Exception {
        statisticalOperationsServiceFacade.findAllInstances(getServiceContextTecnicoApoyoDifusion());
        statisticalOperationsServiceFacade.findAllInstances(getServiceContextTecnicoApoyoPlanificacion());
        statisticalOperationsServiceFacade.findAllInstances(getServiceContextTecnicoApoyoProduccion());
        statisticalOperationsServiceFacade.findAllInstances(getServiceContextTecnicoDifusion());
        statisticalOperationsServiceFacade.findAllInstances(getServiceContextTecnicoPlanificacion());
        statisticalOperationsServiceFacade.findAllInstances(getServiceContextTecnicoProduccion());
    }

    @Test
    public void testFindInstanceByCondition() throws Exception {
        MetamacCriteria criteria = new MetamacCriteria();

        statisticalOperationsServiceFacade.findInstanceByCondition(getServiceContextTecnicoApoyoDifusion(), criteria);
        statisticalOperationsServiceFacade.findInstanceByCondition(getServiceContextTecnicoApoyoPlanificacion(), criteria);
        statisticalOperationsServiceFacade.findInstanceByCondition(getServiceContextTecnicoApoyoProduccion(), criteria);
        statisticalOperationsServiceFacade.findInstanceByCondition(getServiceContextTecnicoDifusion(), criteria);
        statisticalOperationsServiceFacade.findInstanceByCondition(getServiceContextTecnicoPlanificacion(), criteria);
        statisticalOperationsServiceFacade.findInstanceByCondition(getServiceContextTecnicoProduccion(), criteria);
    }

    @Test
    public void testFindInstanceById() throws Exception {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);
        Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();

        statisticalOperationsServiceFacade.findInstanceById(getServiceContextTecnicoApoyoDifusion(), instanceId);
        statisticalOperationsServiceFacade.findInstanceById(getServiceContextTecnicoApoyoPlanificacion(), instanceId);
        statisticalOperationsServiceFacade.findInstanceById(getServiceContextTecnicoApoyoProduccion(), instanceId);
        statisticalOperationsServiceFacade.findInstanceById(getServiceContextTecnicoDifusion(), instanceId);
        statisticalOperationsServiceFacade.findInstanceById(getServiceContextTecnicoPlanificacion(), instanceId);
        statisticalOperationsServiceFacade.findInstanceById(getServiceContextTecnicoProduccion(), instanceId);

    }

    @Test
    public void testFindInstanceByCode() throws Exception {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);
        String instanceCode = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getCode();

        statisticalOperationsServiceFacade.findInstanceByCode(getServiceContextTecnicoApoyoDifusion(), instanceCode);
        statisticalOperationsServiceFacade.findInstanceByCode(getServiceContextTecnicoApoyoPlanificacion(), instanceCode);
        statisticalOperationsServiceFacade.findInstanceByCode(getServiceContextTecnicoApoyoProduccion(), instanceCode);
        statisticalOperationsServiceFacade.findInstanceByCode(getServiceContextTecnicoDifusion(), instanceCode);
        statisticalOperationsServiceFacade.findInstanceByCode(getServiceContextTecnicoPlanificacion(), instanceCode);
        statisticalOperationsServiceFacade.findInstanceByCode(getServiceContextTecnicoProduccion(), instanceCode);
    }

    @Test
    public void testFindInstanceByUrn() throws Exception {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);
        String instanceUrn = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getUrn();

        statisticalOperationsServiceFacade.findInstanceByUrn(getServiceContextTecnicoApoyoDifusion(), instanceUrn);
        statisticalOperationsServiceFacade.findInstanceByUrn(getServiceContextTecnicoApoyoPlanificacion(), instanceUrn);
        statisticalOperationsServiceFacade.findInstanceByUrn(getServiceContextTecnicoApoyoProduccion(), instanceUrn);
        statisticalOperationsServiceFacade.findInstanceByUrn(getServiceContextTecnicoDifusion(), instanceUrn);
        statisticalOperationsServiceFacade.findInstanceByUrn(getServiceContextTecnicoPlanificacion(), instanceUrn);
        statisticalOperationsServiceFacade.findInstanceByUrn(getServiceContextTecnicoProduccion(), instanceUrn);

    }

    @Test
    public void testFindInstanceBaseById() throws Exception {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);
        Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();

        statisticalOperationsServiceFacade.findInstanceBaseById(getServiceContextTecnicoApoyoDifusion(), instanceId);
        statisticalOperationsServiceFacade.findInstanceBaseById(getServiceContextTecnicoApoyoPlanificacion(), instanceId);
        statisticalOperationsServiceFacade.findInstanceBaseById(getServiceContextTecnicoApoyoProduccion(), instanceId);
        statisticalOperationsServiceFacade.findInstanceBaseById(getServiceContextTecnicoDifusion(), instanceId);
        statisticalOperationsServiceFacade.findInstanceBaseById(getServiceContextTecnicoPlanificacion(), instanceId);
        statisticalOperationsServiceFacade.findInstanceBaseById(getServiceContextTecnicoProduccion(), instanceId);

    }

    @Test
    public void testPublishInternallyInstance() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        // getServiceContextTecnicoPlanificacion
        {
            Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();
            statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextTecnicoPlanificacion(), instanceId);
        }

        // getServiceContextTecnicoProduccion
        {
            Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();
            statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextTecnicoProduccion(), instanceId);
        }

        // getServiceContextTecnicoProduccionOperation01
        {
            Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();
            statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextTecnicoProduccionOperation01(), instanceId);
        }

        // Not allowed roles
        {
            Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();

            try {
                statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextTecnicoApoyoPlanificacion(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }

            try {
                statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextTecnicoApoyoProduccion(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }

            try {
                statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextTecnicoApoyoDifusion(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }
            try {
                statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextTecnicoDifusion(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }
            try {
                statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextTecnicoProduccionOperation02(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_ACCESS_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }
        }

    }

    @Test
    public void testPublishExternallyInstance() throws Exception {
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto.setCode("C0025A");
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);
        statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationId);

        // getServiceContextTecnicoPlanificacion
        {
            Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();
            statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextAdministrador(), instanceId);
            statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContextTecnicoPlanificacion(), instanceId);
        }

        // getServiceContextTecnicoProduccion
        {
            Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();
            statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextAdministrador(), instanceId);
            statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContextTecnicoProduccion(), instanceId);
        }

        // getServiceContextTecnicoProduccionOperation01
        {
            Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();
            statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextAdministrador(), instanceId);
            statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContextTecnicoProduccionOperation01(), instanceId);
        }

        // Not allowed roles
        {
            Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();
            statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextAdministrador(), instanceId);

            try {
                statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContextTecnicoApoyoPlanificacion(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }

            try {
                statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContextTecnicoApoyoProduccion(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }

            try {
                statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContextTecnicoApoyoDifusion(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }
            try {
                statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContextTecnicoDifusion(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }
            try {
                statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContextTecnicoProduccionOperation02(), instanceId);
            } catch (MetamacException e) {
                assertEquals(ServiceExceptionType.SECURITY_ACCESS_OPERATION_NOT_ALLOWED.getCode(), e.getExceptionItems().get(0).getCode());
            }
        }

    }

    @Test
    public void testFindOperationForInstance() throws Exception {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);
        Long instanceId = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto()).getId();

        statisticalOperationsServiceFacade.findOperationForInstance(getServiceContextTecnicoApoyoDifusion(), instanceId);
        statisticalOperationsServiceFacade.findOperationForInstance(getServiceContextTecnicoApoyoPlanificacion(), instanceId);
        statisticalOperationsServiceFacade.findOperationForInstance(getServiceContextTecnicoApoyoProduccion(), instanceId);
        statisticalOperationsServiceFacade.findOperationForInstance(getServiceContextTecnicoDifusion(), instanceId);
        statisticalOperationsServiceFacade.findOperationForInstance(getServiceContextTecnicoPlanificacion(), instanceId);
        statisticalOperationsServiceFacade.findOperationForInstance(getServiceContextTecnicoProduccion(), instanceId);
    }

    /**************************************************************************
     * PRIVATE UTILS
     **************************************************************************/

    private FamilyDto createFamilyDto() {
        FamilyDto familyDto = new FamilyDto();

        // Auditable information
        familyDto.setCreatedBy("Junit");
        familyDto.setCreatedDate(new Date());

        // Identifier
        familyDto.setCode("PRUEBA01" + RandomStringUtils.random(50, true, true));

        // TITLE
        InternationalStringDto title = new InternationalStringDto();
        LocalisedStringDto title_es = new LocalisedStringDto();
        title_es.setLabel("Título en español de familia");
        title_es.setLocale("es");
        LocalisedStringDto title_en = new LocalisedStringDto();
        title_en.setLabel("Título en inglés de familia");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        familyDto.setTitle(title);

        // DESCRIPTION
        InternationalStringDto description = new InternationalStringDto();
        LocalisedStringDto description_es = new LocalisedStringDto();
        description_es.setLabel("Descripción en español de familia");
        description_es.setLocale("es");
        LocalisedStringDto description_en = new LocalisedStringDto();
        description_en.setLabel("Descripción en inglés de familia");
        description_en.setLocale("en");
        description.addText(description_es);
        description.addText(description_en);
        familyDto.setDescription(description);

        return familyDto;
    }

    private OperationDto createOperationDto() throws MetamacException {
        OperationDto operationDto = new OperationDto();

        // Auditable information
        operationDto.setCreatedBy("Junit");
        operationDto.setCreatedDate(new Date());

        // Identifier
        operationDto.setCode("PRUEBA01" + RandomStringUtils.random(50, true, true));

        // TITLE
        InternationalStringDto title = new InternationalStringDto();
        LocalisedStringDto title_es = new LocalisedStringDto();
        title_es.setLabel("Título en español de operacion");
        title_es.setLocale("es");
        LocalisedStringDto title_en = new LocalisedStringDto();
        title_en.setLabel("Título en inglés de operacion");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        operationDto.setTitle(title);

        // DESCRIPTION
        InternationalStringDto acronym = new InternationalStringDto();
        LocalisedStringDto acronym_es = new LocalisedStringDto();
        acronym_es.setLabel("Descripción en español de operacion");
        acronym_es.setLocale("es");
        LocalisedStringDto acronym_en = new LocalisedStringDto();
        acronym_en.setLabel("Descripción en inglés de operacion");
        acronym_en.setLocale("en");
        acronym.addText(acronym_es);
        acronym.addText(acronym_en);
        operationDto.setAcronym(acronym);

        // RELEASE_CALENDAR
        operationDto.setReleaseCalendar(false);

        // RELEASE_CALENDAR_ACCESS
        operationDto.setReleaseCalendarAccess("http://www.draft.com");

        // SURVEY_TYPE
        operationDto.setSurveyType(statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextAdministrador(), Long.valueOf(1)));

        // OFFICIALITY_TYPE
        operationDto.setOfficialityType(statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContextAdministrador(), Long.valueOf(1)));

        // SUBJECT_AREA
        operationDto.setSubjectArea(new ExternalItemDto("HEALTH", "/uri/test/category", "URN:CATEGORY:HEALTH", TypeExternalArtefactsEnum.CATEGORY));

        // STATUS
        operationDto.setStatus(StatusEnum.PLANNING);

        // INDICATOR_SYSTEM
        operationDto.setIndicatorSystem(false);

        return operationDto;
    }

    private OperationDto createOperationDtoForInternalPublishing() throws MetamacException {
        OperationDto operationDto = createOperationDto();

        // OBJECTIVE
        InternationalStringDto objective = new InternationalStringDto();
        LocalisedStringDto objective_es = new LocalisedStringDto();
        objective_es.setLabel("OPERACION - OBJECTIVE - ES");
        objective_es.setLocale("es");
        LocalisedStringDto objective_en = new LocalisedStringDto();
        objective_en.setLabel("OPERACION - OBJECTIVE - EN");
        objective_en.setLocale("en");
        objective.addText(objective_es);
        objective.addText(objective_en);
        operationDto.setObjective(objective);

        // SURVEY_TYPE
        operationDto.setSurveyType(statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextAdministrador(), Long.valueOf(1)));

        // PRODUCER
        operationDto.addProducer(new ExternalItemDto("ISTAC", "/uri/test/agency", "URN:ISTAC", TypeExternalArtefactsEnum.AGENCY));
        operationDto.addProducer(new ExternalItemDto("INE", "/uri/test/agency", "URN:INE", TypeExternalArtefactsEnum.AGENCY));

        // REGIONAL_RESPONSIBLE
        operationDto.addRegionalResponsible(new ExternalItemDto("ISTAC", "/uri/test/agency", "URN:ISTAC", TypeExternalArtefactsEnum.AGENCY));

        // PUBLISHER
        operationDto.addPublisher(new ExternalItemDto("ISTAC", "/uri/test/agency", "URN:ISTAC", TypeExternalArtefactsEnum.AGENCY));

        // COMMON_METADATA
        operationDto.setCommonMetadata(new ExternalItemDto("ISTAC", "/uri/test/agency", "URN:ISTAC", TypeExternalArtefactsEnum.AGENCY));

        return operationDto;
    }

    private InstanceDto createInstanceDto() throws MetamacException {
        InstanceDto instanceDto = new InstanceDto();

        // Auditable information
        instanceDto.setCreatedBy("Junit");
        instanceDto.setCreatedDate(new Date());

        // Identifier
        instanceDto.setCode("PRUEBA01" + RandomStringUtils.random(50, true, true));

        // TITLE
        InternationalStringDto title = new InternationalStringDto();
        LocalisedStringDto title_es = new LocalisedStringDto();
        title_es.setLabel("Título en español de operacion");
        title_es.setLocale("es");
        LocalisedStringDto title_en = new LocalisedStringDto();
        title_en.setLabel("Título en inglés de operacion");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        instanceDto.setTitle(title);

        // INSTANCE_TYPE
        instanceDto.setInstanceType(statisticalOperationsServiceFacade.findInstanceTypeById(getServiceContextAdministrador(), Long.valueOf(1)));

        return instanceDto;
    }
}
