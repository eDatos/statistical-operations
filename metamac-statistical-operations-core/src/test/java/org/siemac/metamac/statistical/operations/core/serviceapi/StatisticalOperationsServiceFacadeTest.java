package org.siemac.metamac.statistical.operations.core.serviceapi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang.RandomStringUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.siemac.metamac.core.common.criteria.MetamacCriteria;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaPaginator;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaResult;
import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.core.common.dto.LocalisedStringDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.vo.domain.ExternalItemRepository;
import org.siemac.metamac.domain.statistical.operations.dto.CollMethodDto;
import org.siemac.metamac.domain.statistical.operations.dto.CostDto;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OfficialityTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveySourceDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveyTypeDto;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.domain.statistical.operations.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionParameters;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Spring based transactional test with DbUnit support.
 */

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:oracle/applicationContext-test.xml"})
public class StatisticalOperationsServiceFacadeTest extends StatisticalOperationsBaseTest implements StatisticalOperationsServiceFacadeTestBase {

    @Autowired
    protected StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    @Autowired
    protected ExternalItemRepository             externalItemRepository;

    /**************************************************************************
     * Survey Type
     **************************************************************************/

    @Test
    public void testFindAllSurveyTypes() throws MetamacException {
        List<SurveyTypeDto> surveyTypesList = statisticalOperationsServiceFacade.findAllSurveyTypes(getServiceContextAdministrador());
        assertTrue(!surveyTypesList.isEmpty());
    }

    @Test
    public void testFindSurveyTypeById() throws MetamacException {
        SurveyTypeDto surveyTypeDto = statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextAdministrador(), Long.valueOf(1));
        assertNotNull(surveyTypeDto);
    }

    /**************************************************************************
     * Instance Type
     **************************************************************************/

    @Test
    public void testFindAllInstanceTypes() throws MetamacException {
        List<InstanceTypeDto> instanceTypesList = statisticalOperationsServiceFacade.findAllInstanceTypes(getServiceContextAdministrador());
        assertTrue(!instanceTypesList.isEmpty());
    }

    @Test
    public void testFindInstanceTypeById() throws MetamacException {
        InstanceTypeDto instanceTypeDto = statisticalOperationsServiceFacade.findInstanceTypeById(getServiceContextAdministrador(), Long.valueOf(1));
        assertNotNull(instanceTypeDto);
    }

    /**************************************************************************
     * Survey Source
     **************************************************************************/

    @Test
    public void testFindAllSurveySources() throws MetamacException {
        List<SurveySourceDto> surveySourcesList = statisticalOperationsServiceFacade.findAllSurveySources(getServiceContextAdministrador());
        assertTrue(!surveySourcesList.isEmpty());
    }

    @Test
    public void testFindSurveySourceById() throws MetamacException {
        SurveySourceDto surveySourceDto = statisticalOperationsServiceFacade.findSurveySourceById(getServiceContextAdministrador(), Long.valueOf(1));
        assertNotNull(surveySourceDto);
    }

    /**************************************************************************
     * Officiality Type
     **************************************************************************/

    @Test
    public void testFindAllOfficialityTypes() throws MetamacException {
        List<OfficialityTypeDto> officialityTypesList = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextAdministrador());
        assertTrue(!officialityTypesList.isEmpty());
    }

    @Test
    public void testFindOfficialityTypeById() throws MetamacException {
        OfficialityTypeDto officialityTypeDto = statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContextAdministrador(), Long.valueOf(1));
        assertNotNull(officialityTypeDto);
    }

    /**************************************************************************
     * Coll Methods
     **************************************************************************/

    @Test
    public void testFindAllCollMethods() throws Exception {
        List<CollMethodDto> collMethodsList = statisticalOperationsServiceFacade.findAllCollMethods(getServiceContextAdministrador());
        assertTrue(!collMethodsList.isEmpty());

    }

    @Test
    public void testFindCollMethodById() throws Exception {
        CollMethodDto collMethodDto = statisticalOperationsServiceFacade.findCollMethodById(getServiceContextAdministrador(), Long.valueOf(1));
        assertNotNull(collMethodDto);

    }

    /**************************************************************************
     * Costs
     **************************************************************************/

    @Test
    public void testFindAllCosts() throws Exception {
        List<CostDto> costsList = statisticalOperationsServiceFacade.findAllCosts(getServiceContextAdministrador());
        assertTrue(!costsList.isEmpty());

    }

    @Test
    public void testFindCostById() throws Exception {
        CostDto costDto = statisticalOperationsServiceFacade.findCostById(getServiceContextAdministrador(), Long.valueOf(1));
        assertNotNull(costDto);

    }

    /**************************************************************************
     * Family
     **************************************************************************/

    @Test
    public void testCreateFamily() throws MetamacException {
        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        FamilyDto familyDto = createFamilyDto();

        familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), familyDto);
        assertNotNull(familyDto);

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertEquals(familiesBefore + 1, familiesAfter);
    }

    @Test
    public void testCreateFamilyDuplicatedCode() throws MetamacException {
        FamilyDto persistedFamilyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());

        FamilyDto familyDto = createFamilyDto();
        familyDto.setCode(persistedFamilyDto.getCode());

        try {
            statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), familyDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.FAMILY_ALREADY_EXIST_CODE_DUPLICATED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testUpdateFamily() throws MetamacException {
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDtoForUpdate());

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        // TITLE
        InternationalStringDto title = new InternationalStringDto();
        LocalisedStringDto title_es = new LocalisedStringDto();
        title_es.setLabel("MOD Título en español de familia");
        title_es.setLocale("es");
        LocalisedStringDto title_en = new LocalisedStringDto();
        title_en.setLabel("MOD Título en inglés de familia");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        familyDto.setTitle(title);

        // DESCRIPTION
        InternationalStringDto description = new InternationalStringDto();
        LocalisedStringDto description_es = new LocalisedStringDto();
        description_es.setLabel("MOD Descripción en español de familia");
        description_es.setLocale("es");
        LocalisedStringDto description_en = new LocalisedStringDto();
        description_en.setLabel("MOD Descripción en inglés de familia");
        description_en.setLocale("en");
        description.addText(description_es);
        description.addText(description_en);
        familyDto.setDescription(description);

        familyDto = statisticalOperationsServiceFacade.updateFamily(getServiceContextAdministrador(), familyDto);

        assertNotNull(familyDto);

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertEquals(familiesBefore, familiesAfter);
    }

    @Test
    public void testUpdateFamilyWithoutDescription() throws MetamacException {
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDtoForUpdate());

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        // TITLE
        InternationalStringDto title = new InternationalStringDto();
        LocalisedStringDto title_es = new LocalisedStringDto();
        title_es.setLabel("MOD Título en español de familia");
        title_es.setLocale("es");
        LocalisedStringDto title_en = new LocalisedStringDto();
        title_en.setLabel("MOD Título en inglés de familia");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        familyDto.setTitle(title);

        // DESCRIPTION
        familyDto.setDescription(null);

        familyDto = statisticalOperationsServiceFacade.updateFamily(getServiceContextAdministrador(), familyDto);

        assertNotNull(familyDto);

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertEquals(familiesBefore, familiesAfter);
    }

    @Test
    public void testUpdateFamilyWithoutLocalisedString() throws MetamacException {
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDtoForUpdate());

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        // TITLE
        InternationalStringDto title = new InternationalStringDto();
        LocalisedStringDto title_es = new LocalisedStringDto();
        title_es.setLabel("MOD Título en español de familia");
        title_es.setLocale("es");
        title.addText(title_es);
        familyDto.setTitle(title);

        // DESCRIPTION
        InternationalStringDto description = new InternationalStringDto();
        LocalisedStringDto description_es = new LocalisedStringDto();
        description_es.setLabel("MOD Descripción en español de familia");
        description_es.setLocale("es");
        description.addText(description_es);
        familyDto.setDescription(description);

        familyDto = statisticalOperationsServiceFacade.updateFamily(getServiceContextAdministrador(), familyDto);

        assertNotNull(familyDto);

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertEquals(familiesBefore, familiesAfter);
    }

    @Test
    public void testUpdateFamilyWithOperations() throws MetamacException {
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDtoForUpdate());

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        OperationDto operation01 = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operation01.getId());

        OperationDto operation02 = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operation02.getId());

        // Check operations for family
        List<OperationBaseDto> operationsForFamilyBefore = statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContextAdministrador(), familyDto.getId());
        assertEquals(2, operationsForFamilyBefore.size());

        // Get family after modified
        familyDto = statisticalOperationsServiceFacade.findFamilyById(getServiceContextAdministrador(), familyDto.getId());

        // TITLE
        InternationalStringDto title = new InternationalStringDto();
        LocalisedStringDto title_es = new LocalisedStringDto();
        title_es.setLabel("MOD Título en español de familia");
        title_es.setLocale("es");
        LocalisedStringDto title_en = new LocalisedStringDto();
        title_en.setLabel("MOD Título en inglés de familia");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        familyDto.setTitle(title);

        // DESCRIPTION
        InternationalStringDto description = new InternationalStringDto();
        LocalisedStringDto description_es = new LocalisedStringDto();
        description_es.setLabel("MOD Descripción en español de familia");
        description_es.setLocale("es");
        LocalisedStringDto description_en = new LocalisedStringDto();
        description_en.setLabel("MOD Descripción en inglés de familia");
        description_en.setLocale("en");
        description.addText(description_es);
        description.addText(description_en);
        familyDto.setDescription(description);

        familyDto = statisticalOperationsServiceFacade.updateFamily(getServiceContextAdministrador(), familyDto);

        assertNotNull(familyDto);

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertEquals(familiesBefore, familiesAfter);

        // Check operations for family
        List<OperationBaseDto> operationsForFamilyAfter = statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContextAdministrador(), familyDto.getId());
        assertEquals(operationsForFamilyBefore.size(), operationsForFamilyAfter.size());
    }

    @Test
    public void testUpdateFamilyOptimisticLockingError() throws Exception {
        Long id = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDtoForUpdate()).getId();

        // Retrieve family - session 1
        FamilyDto familyDtoSession1 = statisticalOperationsServiceFacade.findFamilyById(getServiceContextAdministrador(), id);
        assertEquals(Long.valueOf(0), familyDtoSession1.getOptimisticLockingVersion());
        familyDtoSession1.setCode("newCode");

        // Retrieve family - session 2
        FamilyDto familyDtoSession2 = statisticalOperationsServiceFacade.findFamilyById(getServiceContextAdministrador(), id);
        assertEquals(Long.valueOf(0), familyDtoSession1.getOptimisticLockingVersion());
        familyDtoSession2.setCode("newCode2");

        // Update family - session 1
        FamilyDto familyDtoSession1AfterUpdate = statisticalOperationsServiceFacade.updateFamily(getServiceContextAdministrador(), familyDtoSession1);
        assertEquals(Long.valueOf(1), familyDtoSession1AfterUpdate.getOptimisticLockingVersion());

        // Update family - session 2
        try {
            statisticalOperationsServiceFacade.updateFamily(getServiceContextAdministrador(), familyDtoSession2);
            fail("Optimistic locking");
        } catch (MetamacException e) {
            assertEquals(1, e.getExceptionItems().size());
            assertEquals(ServiceExceptionType.OPTIMISTIC_LOCKING.getCode(), e.getExceptionItems().get(0).getCode());
            assertNull(e.getExceptionItems().get(0).getMessageParameters());
        }

        // Update family - session 1
        familyDtoSession1AfterUpdate.setCode("newCode1_secondUpdate");
        FamilyDto familyDtoSession1AfterUpdate2 = statisticalOperationsServiceFacade.updateFamily(getServiceContextAdministrador(), familyDtoSession1AfterUpdate);
        assertEquals(Long.valueOf(2), familyDtoSession1AfterUpdate2.getOptimisticLockingVersion());
    }

    @Test
    public void testDeleteFamily() throws MetamacException {
        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDtoForUpdate());
        assertNotNull(familyDto);
        int numberFamilies = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        // Remove family
        statisticalOperationsServiceFacade.deleteFamily(getServiceContextAdministrador(), familyDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size() == (numberFamilies - 1));

        try {
            statisticalOperationsServiceFacade.findFamilyById(getServiceContextAdministrador(), familyDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.FAMILY_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testDeleteFamilyWithOperations() throws MetamacException {
        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDtoForUpdate());
        assertNotNull(familyDto);
        int numberFamilies = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        // Create operations
        OperationDto operation01 = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operation01.getId());

        OperationDto operation02 = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operation02.getId());

        // Check number of operations before delete
        int operationsBeforeDelete = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        // Remove family
        statisticalOperationsServiceFacade.deleteFamily(getServiceContextAdministrador(), familyDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size() == (numberFamilies - 1));

        // Check for deleted family
        try {
            statisticalOperationsServiceFacade.findFamilyById(getServiceContextAdministrador(), familyDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.FAMILY_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check number of operations after delete
        int operationsAfterDelete = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(operationsBeforeDelete, operationsAfterDelete);
    }

    @Test
    public void testDeleteFamilyInternallyPublished() throws MetamacException {
        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        assertNotNull(familyDto);

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        // Set PUBLISH_INTERNALLY procStatus
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operationDto.getId());

        familyDto = statisticalOperationsServiceFacade.findFamilyById(getServiceContextAdministrador(), familyDto.getId());
        familyDto = statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextAdministrador(), familyDto.getId());

        // Delete family with an incorrect procStatus
        try {
            statisticalOperationsServiceFacade.deleteFamily(getServiceContextAdministrador(), familyDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }
        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertEquals(familiesBefore, familiesAfter);

    }

    @Test
    public void testDeleteFamilyExternallyPublished() throws MetamacException {
        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        assertNotNull(familyDto);

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        // Set operation procStatus
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operationDto.getId());

        // Set PUBLISH_EXTERNALLY procStatus
        familyDto = statisticalOperationsServiceFacade.findFamilyById(getServiceContextAdministrador(), familyDto.getId());
        familyDto = statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextAdministrador(), familyDto.getId());
        familyDto = statisticalOperationsServiceFacade.publishExternallyFamily(getServiceContextAdministrador(), familyDto.getId());

        // Delete family with an incorrect procStatus
        try {
            statisticalOperationsServiceFacade.deleteFamily(getServiceContextAdministrador(), familyDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertEquals(familiesBefore, familiesAfter);
    }

    @Test
    public void testFindAllFamilies() throws MetamacException {
        statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());

        List<FamilyBaseDto> families = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador());
        assertTrue(!families.isEmpty());

    }

    @Test
    public void testFindFamilyByCondition() throws MetamacException {
        statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());

        MetamacCriteria criteria = new MetamacCriteria();
        MetamacCriteriaResult<FamilyBaseDto> result = statisticalOperationsServiceFacade.findFamilyByCondition(getServiceContextAdministrador(), criteria);
        assertTrue(result.getResults().size() >= 2);
    }

    @Test
    public void testFindFamilyByConditionPaginated() throws MetamacException {
        statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());

        MetamacCriteria criteria = new MetamacCriteria();

        MetamacCriteriaPaginator paginator = new MetamacCriteriaPaginator();
        paginator.setCountTotalResults(Boolean.TRUE);
        paginator.setMaximumResultSize(Integer.valueOf(2));
        criteria.setPaginator(paginator);

        {
            // Page 1
            criteria.getPaginator().setFirstResult(Integer.valueOf(0));

            MetamacCriteriaResult<FamilyBaseDto> result = statisticalOperationsServiceFacade.findFamilyByCondition(getServiceContextAdministrador(), criteria);
            assertEquals(2, result.getResults().size());
            assertTrue(result.getPaginatorResult().getTotalResults() >= 3);
            assertEquals(Integer.valueOf(0), result.getPaginatorResult().getFirstResult());
        }

        {
            // Page 2
            criteria.getPaginator().setFirstResult(Integer.valueOf(2));
            MetamacCriteriaResult<FamilyBaseDto> result = statisticalOperationsServiceFacade.findFamilyByCondition(getServiceContextAdministrador(), criteria);
            assertTrue(result.getPaginatorResult().getTotalResults() >= 1);
            assertEquals(Integer.valueOf(2), result.getPaginatorResult().getFirstResult());

        }
    }

    @Test
    public void testFindFamilyById() throws MetamacException {
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());

        FamilyDto familyRetrieved = statisticalOperationsServiceFacade.findFamilyById(getServiceContextAdministrador(), familyDto.getId());

        assertNotNull(familyRetrieved);
        assertTrue(familyDto.getId().equals(familyRetrieved.getId()));
    }

    @Test
    public void testPublishInternallyFamilyError() throws MetamacException {
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operationDto.getId());

        familyDto = statisticalOperationsServiceFacade.findFamilyById(getServiceContextAdministrador(), familyDto.getId());

        try {
            familyDto = statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextAdministrador(), familyDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.FAMILY_WITHOUT_PUBLISHED_INTERNALLY_OPERATIONS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertTrue(familiesBefore == familiesAfter);
    }

    @Test
    public void testPublishInternallyFamily() throws MetamacException {
        // Create operation with ProcStatus PUBLISH_EXTERNALLY
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto);
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        assertNotNull(operationDto.getInternalInventoryDate());
        assertEquals(ProcStatusEnum.PUBLISH_INTERNALLY, operationDto.getProcStatus());

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        // Associate
        List<OperationBaseDto> operationsForFamily = statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operationDto.getId());
        assertNotNull(operationsForFamily);

        // Reload family
        familyDto = statisticalOperationsServiceFacade.findFamilyById(getServiceContextAdministrador(), familyDto.getId());

        // Publish family
        familyDto = statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextAdministrador(), familyDto.getId());

        // Validations
        assertNotNull(familyDto);
        assertNotNull(familyDto.getInternalInventoryDate());
        assertTrue(ProcStatusEnum.PUBLISH_INTERNALLY.equals(familyDto.getProcStatus()));
        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertTrue(familiesBefore == familiesAfter);
    }

    @Test
    public void testPublishExternallyFamily() throws MetamacException {

        // Create operation with ProcStatus PUBLISH_EXTERNALLY
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        assertNotNull(operationDto.getInternalInventoryDate());
        assertNotNull(operationDto.getInventoryDate());
        assertEquals(ProcStatusEnum.PUBLISH_EXTERNALLY, operationDto.getProcStatus());

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        // Associate
        List<OperationBaseDto> operationsForFamily = statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operationDto.getId());
        assertNotNull(operationsForFamily);

        // Reload family
        familyDto = statisticalOperationsServiceFacade.findFamilyById(getServiceContextAdministrador(), familyDto.getId());

        // Publish family
        familyDto = statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextAdministrador(), familyDto.getId());
        familyDto = statisticalOperationsServiceFacade.publishExternallyFamily(getServiceContextAdministrador(), familyDto.getId());
        assertTrue(ProcStatusEnum.PUBLISH_EXTERNALLY.equals(familyDto.getProcStatus()));

        // Validations
        assertNotNull(familyDto);
        assertNotNull(familyDto.getInternalInventoryDate());
        assertNotNull(familyDto.getInventoryDate());
        assertTrue(ProcStatusEnum.PUBLISH_EXTERNALLY.equals(familyDto.getProcStatus()));
        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertEquals(familiesBefore, familiesAfter);

        // Check that family can't be internally published
        try {
            statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContextAdministrador(), familyDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testFindOperationsForFamily() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        assertNotNull(operationDto);

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        assertNotNull(familyDto);

        // Associate
        List<OperationBaseDto> operationsForFamily = statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operationDto.getId());

        assertNotNull(operationsForFamily);
    }

    @Test
    public void testAddOperationForFamily() throws MetamacException {

        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        assertNotNull(operationDto);

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        assertNotNull(familyDto);

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        // Associate
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operationDto.getId());

        assertNotNull(statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContextAdministrador(), familyDto.getId()));

        // Check number of families
        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertEquals(familiesBefore, familiesAfter);
    }

    @Test
    public void testRemoveOperationForFamily() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        assertNotNull(operationDto);

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        assertNotNull(familyDto);

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        // Associate
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operationDto.getId());

        assertNotNull(statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContextAdministrador(), familyDto.getId()));

        // Remove
        statisticalOperationsServiceFacade.removeOperationForFamily(getServiceContextAdministrador(), familyDto.getId(), operationDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContextAdministrador(), familyDto.getId()).isEmpty());

        // Check number of families
        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertEquals(familiesBefore, familiesAfter);
    }

    /**************************************************************************
     * Operation
     **************************************************************************/

    @Test
    public void testCreateOperation() throws MetamacException {

        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        assertNotNull(operationDto);

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(1, operationsAfter);
    }

    @Test
    public void testCreateOperationDuplicatedCode() throws MetamacException {
        OperationDto persistedOperationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());

        OperationDto operationDto = createOperationDto();
        operationDto.setCode(persistedOperationDto.getCode());

        try {
            statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.OPERATION_ALREADY_EXIST_CODE_DUPLICATED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testUpdateOperation() throws Exception {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        assertNotNull(operationDto);

        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        // TITLE
        InternationalStringDto title = new InternationalStringDto();
        LocalisedStringDto title_es = new LocalisedStringDto();
        title_es.setLabel("MOD Título en español de operacion");
        title_es.setLocale("es");
        LocalisedStringDto title_en = new LocalisedStringDto();
        title_en.setLabel("MOD Título en inglés de operacion");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        operationDto.setTitle(title);

        // DESCRIPTION
        InternationalStringDto description = new InternationalStringDto();
        LocalisedStringDto description_es = new LocalisedStringDto();
        description_es.setLabel("MOD Descripción en español de operacion");
        description_es.setLocale("es");
        LocalisedStringDto description_en = new LocalisedStringDto();
        description_en.setLabel("MOD Descripción en inglés de operacion");
        description_en.setLocale("en");
        description.addText(description_es);
        description.addText(description_en);
        operationDto.setDescription(description);

        operationDto.setReleaseCalendarAccess(null);

        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDto);

        assertNotNull(operationDto);

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testUpdateOperationWithDescriptionWithoutLocales() throws Exception {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        assertNotNull(operationDto);

        // DESCRIPTION
        InternationalStringDto description = new InternationalStringDto();
        operationDto.setDescription(description);

        try {
            statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.METADATA_REQUIRED.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(ServiceExceptionParameters.OPERATION_DESCRIPTION, e.getExceptionItems().get(0).getMessageParameters()[0]);
        }

    }

    @Test
    public void testUpdateOperationStatus() throws Exception {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        assertNotNull(operationDto);

        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        // STATUS
        operationDto.setStatus(StatusEnum.DESIGN);

        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDto);

        assertNotNull(operationDto);
        assertEquals(StatusEnum.DESIGN, operationDto.getStatus());

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testUpdateOperationWithExternalItems() throws Exception {
        // Create operation
        int externalItemsBefore = externalItemRepository.findAll().size();

        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoWithProducer());

        int externalItemsAfter = externalItemRepository.findAll().size();
        assertEquals(externalItemsBefore + 2, externalItemsAfter);

        // ADD PRODUCER
        externalItemsBefore = externalItemRepository.findAll().size();

        ExternalItemBtDto producerUpdated = new ExternalItemBtDto();
        producerUpdated.setCodeId("ISTAC_MOD");
        producerUpdated.setType(TypeExternalArtefactsEnum.AGENCY);
        producerUpdated.setUriInt("uri:internal_mod:todo");
        operationDto.addProducer(producerUpdated);
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDto);

        externalItemsAfter = externalItemRepository.findAll().size();
        assertEquals(externalItemsBefore + 1, externalItemsAfter);

        // PRODUCER: CLEAR AND ADD TWO ELEMENTS
        externalItemsBefore = externalItemRepository.findAll().size();

        operationDto.getProducer().clear();
        ExternalItemBtDto producer01 = new ExternalItemBtDto();
        producer01.setCodeId("ISTAC_FOR_REMOVE");
        producer01.setType(TypeExternalArtefactsEnum.AGENCY);
        producer01.setUriInt("uri:interna:todo");
        operationDto.addProducer(producer01);

        ExternalItemBtDto producer02 = new ExternalItemBtDto();
        producer02.setCodeId("INE_FOR_REMOVE");
        producer02.setType(TypeExternalArtefactsEnum.AGENCY);
        producer02.setUriInt("uri:interna:todo");
        operationDto.addProducer(producer02);
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDto);

        externalItemsAfter = externalItemRepository.findAll().size();
        assertEquals(externalItemsBefore - 1, externalItemsAfter);

        // PRODUCER: REMOVE ALL
        externalItemsBefore = externalItemRepository.findAll().size();

        operationDto.removeAllProducer();
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDto);

        externalItemsAfter = externalItemRepository.findAll().size();
        assertEquals(externalItemsBefore - 2, externalItemsAfter);

    }

    @Test
    public void testUpdateOperationWithoutExternalItemsPreviuslySave() throws Exception {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());

        // ADD REGIONAL CONTRIBUTOR
        int externalItemsBefore = externalItemRepository.findAll().size();

        ExternalItemBtDto regionalContributor01 = new ExternalItemBtDto();
        regionalContributor01.setCodeId("ISTAC_REMOVE");
        regionalContributor01.setType(TypeExternalArtefactsEnum.AGENCY);
        regionalContributor01.setUriInt("uri:internal_remove:todo");
        operationDto.addRegionalContributor(regionalContributor01);

        ExternalItemBtDto regionalContributor02 = new ExternalItemBtDto();
        regionalContributor02.setCodeId("INE_REMOVE");
        regionalContributor02.setType(TypeExternalArtefactsEnum.AGENCY);
        regionalContributor02.setUriInt("uri:internal_remove:todo");
        operationDto.addRegionalContributor(regionalContributor02);

        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDto);

        int externalItemsAfter = externalItemRepository.findAll().size();
        assertEquals(externalItemsBefore + 2, externalItemsAfter);

        // CLEAR REGIONAL CONTRIBUTOR
        externalItemsBefore = externalItemRepository.findAll().size();

        operationDto.getRegionalContributor().clear();
        operationDto.getRegionalContributor().addAll(new ArrayList<ExternalItemBtDto>());

        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDto);

        externalItemsAfter = externalItemRepository.findAll().size();
        assertEquals(externalItemsBefore - 2, externalItemsAfter);

    }

    @Test
    public void testCreateOperationWithIncorrectReleaseCalendarAccess() throws MetamacException {
        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        OperationDto operationDto = createOperationDto();
        operationDto.setReleaseCalendarAccess("INCORRECT URL");

        try {
            operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.METADATA_INVALID_URL.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testUpdateOperationWithList() throws Exception {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoWithOfficialityType());
        assertNotNull(operationDto);
        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        // MODIFY OFFICIALITY TYPE
        int officialityTypesBefore = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextAdministrador()).size();

        operationDto.setOfficialityType(statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContextAdministrador(), Long.valueOf(2)));
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDto);

        int officialityTypesAfter = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextAdministrador()).size();
        assertEquals(officialityTypesBefore, officialityTypesAfter);

        // REMOVE OFFICIALITY TYPE
        officialityTypesBefore = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextAdministrador()).size();

        operationDto.setOfficialityType(null);
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDto);

        officialityTypesAfter = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextAdministrador()).size();
        assertEquals(officialityTypesBefore, officialityTypesAfter);

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(operationsBefore, operationsAfter);
    }
    
    @Test
    public void testUpdateOperationOptimisticLockingError() throws Exception {
        Long id = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto()).getId();

        // Retrieve operation - session 1
        OperationDto operationDtoSession1 = statisticalOperationsServiceFacade.findOperationById(getServiceContextAdministrador(), id);
        assertEquals(Long.valueOf(0), operationDtoSession1.getOptimisticLockingVersion());
        operationDtoSession1.setCode("newCode");

        // Retrieve operation - session 2
        OperationDto operationDtoSession2 = statisticalOperationsServiceFacade.findOperationById(getServiceContextAdministrador(), id);
        assertEquals(Long.valueOf(0), operationDtoSession1.getOptimisticLockingVersion());
        operationDtoSession2.setCode("newCode2");

        // Update operation - session 1
        OperationDto operationDtoSession1AfterUpdate = statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDtoSession1);
        assertEquals(Long.valueOf(1), operationDtoSession1AfterUpdate.getOptimisticLockingVersion());

        // Update operation - session 2
        try {
            statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDtoSession2);
            fail("Optimistic locking");
        } catch (MetamacException e) {
            assertEquals(1, e.getExceptionItems().size());
            assertEquals(ServiceExceptionType.OPTIMISTIC_LOCKING.getCode(), e.getExceptionItems().get(0).getCode());
            assertNull(e.getExceptionItems().get(0).getMessageParameters());
        }

        // Update operation - session 1
        operationDtoSession1AfterUpdate.setCode("newCode1_secondUpdate");
        OperationDto operationDtoSession1AfterUpdate2 = statisticalOperationsServiceFacade.updateOperation(getServiceContextAdministrador(), operationDtoSession1AfterUpdate);
        assertEquals(Long.valueOf(2), operationDtoSession1AfterUpdate2.getOptimisticLockingVersion());
    }

    @Test
    public void testDeleteOperationWithList() throws Exception {
        // Create operation with officiality type
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoWithOfficialityType());
        assertNotNull(operationDto);
        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        // Delete operation
        int officialityTypesBefore = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextAdministrador()).size();

        statisticalOperationsServiceFacade.deleteOperation(getServiceContextAdministrador(), operationDto.getId());

        int officialityTypesAfter = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContextAdministrador()).size();
        assertEquals(officialityTypesBefore, officialityTypesAfter);

        // Retrieve deleted operation
        try {
            statisticalOperationsServiceFacade.findOperationById(getServiceContextAdministrador(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.OPERATION_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(operationsBefore - 1, operationsAfter);
    }

    @Test
    public void testDeleteOperationWithProducer() throws Exception {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        assertNotNull(operationDto);
        int numberOperations = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        statisticalOperationsServiceFacade.deleteOperation(getServiceContextAdministrador(), operationDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size() == (numberOperations - 1));

        try {
            statisticalOperationsServiceFacade.findOperationById(getServiceContextAdministrador(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.OPERATION_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testDeleteOperation() throws MetamacException {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        assertNotNull(operationDto);
        int numberOperations = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        statisticalOperationsServiceFacade.deleteOperation(getServiceContextAdministrador(), operationDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size() == (numberOperations - 1));

        try {
            statisticalOperationsServiceFacade.findOperationById(getServiceContextAdministrador(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.OPERATION_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testDeleteOperationWithFamilies() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        assertNotNull(operationDto);
        int numberOperations = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        // Create families and associate
        FamilyDto family01 = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextAdministrador(), operationDto.getId(), family01.getId());

        FamilyDto family02 = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextAdministrador(), operationDto.getId(), family02.getId());

        // Count number of families before delete
        int familiesBeforeDelete = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();

        // Get operation
        operationDto = statisticalOperationsServiceFacade.findOperationById(getServiceContextAdministrador(), operationDto.getId());

        // Delete Operation
        statisticalOperationsServiceFacade.deleteOperation(getServiceContextAdministrador(), operationDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size() == (numberOperations - 1));

        // Check for deleted operation
        try {
            statisticalOperationsServiceFacade.findOperationById(getServiceContextAdministrador(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.OPERATION_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check number of families before delete
        int familiesAfterDelete = statisticalOperationsServiceFacade.findAllFamilies(getServiceContextAdministrador()).size();
        assertEquals(familiesBeforeDelete, familiesAfterDelete);
    }

    @Test
    public void testDeleteOperationInternallyPublished() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        assertNotNull(operationDto);

        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        // Set PUBLISH_INTERNALLY procStatus
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Delete operation with an incorrect procStatus
        try {
            statisticalOperationsServiceFacade.deleteOperation(getServiceContextAdministrador(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }

        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testDeleteOperationExternallyPublished() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        assertNotNull(operationDto);

        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        // Set PUBLISH_INTERNALLY procStatus
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Delete operation with an incorrect procStatus
        try {
            statisticalOperationsServiceFacade.deleteOperation(getServiceContextAdministrador(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }

        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testFindAllOperations() throws MetamacException {
        statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        List<OperationBaseDto> operations = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador());
        assertTrue(!operations.isEmpty());
    }

    @Test
    public void testFindOperationsByCondition() throws MetamacException {
        statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());

        MetamacCriteria criteria = new MetamacCriteria();
        MetamacCriteriaResult<OperationBaseDto> result = statisticalOperationsServiceFacade.findOperationsByCondition(getServiceContextAdministrador(), criteria);
        assertTrue(result.getResults().size() >= 2);
    }

    @Test
    public void testFindOperationsByConditionPaginated() throws MetamacException {
        statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());

        MetamacCriteria criteria = new MetamacCriteria();

        MetamacCriteriaPaginator paginator = new MetamacCriteriaPaginator();
        paginator.setCountTotalResults(Boolean.TRUE);
        paginator.setMaximumResultSize(Integer.valueOf(2));
        criteria.setPaginator(paginator);

        {
            // Page 1
            criteria.getPaginator().setFirstResult(Integer.valueOf(0));

            MetamacCriteriaResult<OperationBaseDto> result = statisticalOperationsServiceFacade.findOperationsByCondition(getServiceContextAdministrador(), criteria);
            assertEquals(2, result.getResults().size());
            assertTrue(result.getPaginatorResult().getTotalResults() >= 3);
            assertEquals(Integer.valueOf(0), result.getPaginatorResult().getFirstResult());
        }

        {
            // Page 2
            criteria.getPaginator().setFirstResult(Integer.valueOf(2));
            MetamacCriteriaResult<OperationBaseDto> result = statisticalOperationsServiceFacade.findOperationsByCondition(getServiceContextAdministrador(), criteria);
            assertTrue(result.getPaginatorResult().getTotalResults() >= 1);
            assertEquals(Integer.valueOf(2), result.getPaginatorResult().getFirstResult());

        }
    }

    @Test
    public void testFindOperationById() throws MetamacException {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());

        OperationDto operationRetrieved = statisticalOperationsServiceFacade.findOperationById(getServiceContextAdministrador(), operationDto.getId());

        assertNotNull(operationRetrieved);
        assertTrue(operationDto.getId().equals(operationRetrieved.getId()));
    }

    @Test
    public void testPublishInternallyOperation() throws MetamacException {

        // Create and Publish operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Validations
        assertNotNull(operationDto.getInternalInventoryDate());
        assertTrue(ProcStatusEnum.PUBLISH_INTERNALLY.equals(operationDto.getProcStatus()));

        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testPublishExternallyOperation() throws MetamacException {

        // Create and Publish operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Validations
        assertNotNull(operationDto.getInternalInventoryDate());
        assertNotNull(operationDto.getInventoryDate());
        assertTrue(ProcStatusEnum.PUBLISH_EXTERNALLY.equals(operationDto.getProcStatus()));

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(operationsBefore, operationsAfter);

        // Check that operation can't be internally published
        try {
            statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testFindFamiliesForOperation() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        assertNotNull(operationDto);

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        assertNotNull(familyDto);

        // Associate
        List<FamilyBaseDto> familiesForOperation = statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextAdministrador(), operationDto.getId(), familyDto.getId());

        assertNotNull(familiesForOperation);

    }

    @Test
    public void testFindInstancesForOperation() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        assertNotNull(operationDto);

        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        assertNotNull(instanceDto);

        assertEquals(1, statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextAdministrador(), operationDto.getId()).size());
    }

    // @Test
    // public void testFindInstancesForOperationId() throws MetamacException {
    // List<InstanceDto> instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextAdministrador(), Long.valueOf(123));
    //
    // assertNotNull(instances);
    // }

    @Test
    public void testAddFamilyForOperation() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        assertNotNull(operationDto);

        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        assertNotNull(familyDto);

        // Associate
        statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextAdministrador(), operationDto.getId(), familyDto.getId());

        assertNotNull(statisticalOperationsServiceFacade.findFamiliesForOperation(getServiceContextAdministrador(), operationDto.getId()));

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(operationsBefore, operationsAfter);

    }

    @Test
    public void testRemoveFamilyForOperation() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDto());
        assertNotNull(operationDto);

        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContextAdministrador(), createFamilyDto());
        assertNotNull(familyDto);

        // Associate
        statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContextAdministrador(), operationDto.getId(), familyDto.getId());
        assertNotNull(statisticalOperationsServiceFacade.findFamiliesForOperation(getServiceContextAdministrador(), operationDto.getId()));

        // Remove
        statisticalOperationsServiceFacade.removeFamilyForOperation(getServiceContextAdministrador(), operationDto.getId(), familyDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findFamiliesForOperation(getServiceContextAdministrador(), operationDto.getId()).isEmpty());

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    /**************************************************************************
     * Instance
     **************************************************************************/

    @Test
    public void testCreateInstance() throws MetamacException, InterruptedException {
        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();

        // Create operation
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto());
        assertNotNull(instanceDto);

        // Ckeck number of instances in the operation
        List<InstanceBaseDto> operationInstances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextAdministrador(), operationId);
        assertEquals(1, operationInstances.size());

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();
        assertEquals(instancesBefore + 1, instancesAfter);

    }

    @Test
    public void testCreateInstanceDuplicatedCode() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        Long operationId = operationDto.getId();
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Create instance
        InstanceDto persistedIstanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto());

        InstanceDto instanceDto = createInstanceDto();
        instanceDto.setCode(persistedIstanceDto.getCode());

        try {
            statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, instanceDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INSTANCE_ALREADY_EXIST_CODE_DUPLICATED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testCreateInstanceError() throws MetamacException {
        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();

        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Create instance
        try {
            statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createIncompleteInstanceDto());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.METADATA_REQUIRED.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();
        assertEquals(instancesBefore, instancesAfter);

    }

    @Test
    public void testUpdateInstance() throws Exception {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        assertNotNull(instanceDto);

        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();

        // TITLE
        InternationalStringDto title = new InternationalStringDto();
        LocalisedStringDto title_es = new LocalisedStringDto();
        title_es.setLabel("MOD Título en español de instancia");
        title_es.setLocale("es");
        LocalisedStringDto title_en = new LocalisedStringDto();
        title_en.setLabel("MOD Título en inglés de instancia");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        instanceDto.setTitle(title);

        instanceDto = statisticalOperationsServiceFacade.updateInstance(getServiceContextAdministrador(), instanceDto);

        assertNotNull(operationDto);

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();
        assertEquals(instancesBefore, instancesAfter);

    }

    @Test
    public void testUpdateInstanceWithExternalItemBt() throws Exception {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDtoWithGeographicGranularity());
        assertNotNull(instanceDto);

        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();

        // GEOGRAPHIC_GRANULARITY
        instanceDto.setGeographicGranularity(null);

        // Save
        instanceDto = statisticalOperationsServiceFacade.updateInstance(getServiceContextAdministrador(), instanceDto);

        // Validations
        assertNotNull(operationDto);
        assertNotNull(instanceDto);
        assertTrue(instanceDto.getGeographicGranularity() == null);

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();
        assertEquals(instancesBefore, instancesAfter);

    }

    @Test
    public void testUpdateInstancesOrder() throws Exception {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        InstanceDto instance01 = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        InstanceDto instance02 = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        InstanceDto instance03 = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());

        List<Long> instancesIds = new ArrayList<Long>();
        instancesIds.add(instance03.getId());
        instancesIds.add(instance02.getId());
        instancesIds.add(instance01.getId());

        List<InstanceBaseDto> orderedInstances = statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContextAdministrador(), operationDto.getId(), instancesIds);

        // Check correct order
        assertEquals(orderedInstances.get(2).getId(), instance01.getId());
        assertEquals(orderedInstances.get(1).getId(), instance02.getId());
        assertEquals(orderedInstances.get(0).getId(), instance03.getId());

        // Check order number
        // TODO: Check this
        // assertEquals(orderedInstances.get(0).getOrder(), Integer.valueOf(2));
        // assertEquals(orderedInstances.get(1).getOrder(), Integer.valueOf(1));
        // assertEquals(orderedInstances.get(2).getOrder(), Integer.valueOf(0));

        // Check number of instances
        assertEquals(3, statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextAdministrador(), operationDto.getId()).size());
    }
    
    @Test
    public void testUpdateInstanceOptimisticLockingError() throws Exception {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        Long id = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto()).getId();

        // Retrieve instance - session 1
        InstanceDto instanceDtoSession1 = statisticalOperationsServiceFacade.findInstanceById(getServiceContextAdministrador(), id);
        assertEquals(Long.valueOf(0), instanceDtoSession1.getOptimisticLockingVersion());
        instanceDtoSession1.setCode("newCode");

        // Retrieve instance - session 2
        InstanceDto instanceDtoSession2 = statisticalOperationsServiceFacade.findInstanceById(getServiceContextAdministrador(), id);
        assertEquals(Long.valueOf(0), instanceDtoSession1.getOptimisticLockingVersion());
        instanceDtoSession2.setCode("newCode2");

        // Update instance - session 1
        InstanceDto instanceDtoSession1AfterUpdate = statisticalOperationsServiceFacade.updateInstance(getServiceContextAdministrador(), instanceDtoSession1);
        assertEquals(Long.valueOf(1), instanceDtoSession1AfterUpdate.getOptimisticLockingVersion());

        // Update instance - session 2
        try {
            statisticalOperationsServiceFacade.updateInstance(getServiceContextAdministrador(), instanceDtoSession2);
            fail("Optimistic locking");
        } catch (MetamacException e) {
            assertEquals(1, e.getExceptionItems().size());
            assertEquals(ServiceExceptionType.OPTIMISTIC_LOCKING.getCode(), e.getExceptionItems().get(0).getCode());
            assertNull(e.getExceptionItems().get(0).getMessageParameters());
        }

        // Update instance - session 1
        instanceDtoSession1AfterUpdate.setCode("newCode1_secondUpdate");
        InstanceDto instanceDtoSession1AfterUpdate2 = statisticalOperationsServiceFacade.updateInstance(getServiceContextAdministrador(), instanceDtoSession1AfterUpdate);
        assertEquals(Long.valueOf(2), instanceDtoSession1AfterUpdate2.getOptimisticLockingVersion());
    }

    @Test
    public void testDeleteInstance() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        assertNotNull(instanceDto);

        int numberInstances = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();

        statisticalOperationsServiceFacade.deleteInstance(getServiceContextAdministrador(), instanceDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size() == (numberInstances - 1));

        try {
            statisticalOperationsServiceFacade.findInstanceById(getServiceContextAdministrador(), instanceDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INSTANCE_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testCheckInstancesOrderAfterRemoveInstance() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Create instance 01
        InstanceDto instance01 = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        assertNotNull(instance01);

        // Create instance 02
        InstanceDto instance02 = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        assertNotNull(instance02);

        // Check initial order
        List<InstanceBaseDto> instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextAdministrador(), operationDto.getId());
        assertEquals(instance02.getId(), instances.get(0).getId());
        assertEquals(instance01.getId(), instances.get(1).getId());

        assertEquals(Integer.valueOf(1), instances.get(0).getOrder());
        assertEquals(Integer.valueOf(0), instances.get(1).getOrder());

        // Reorder instances
        List<Long> newOrder = new ArrayList<Long>();
        newOrder.add(instance02.getId());
        newOrder.add(instance01.getId());
        statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContextAdministrador(), operationDto.getId(), newOrder);

        // Check order
        instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextAdministrador(), operationDto.getId());
        assertEquals(instance01.getId(), instances.get(0).getId());
        assertEquals(instance02.getId(), instances.get(1).getId());

        assertEquals(Integer.valueOf(0), instances.get(1).getOrder());
        assertEquals(Integer.valueOf(1), instances.get(0).getOrder());

        // Create instance 03
        InstanceDto instance03 = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        assertNotNull(instance03);

        // Check order
        instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextAdministrador(), operationDto.getId());
        assertEquals(instance02.getId(), instances.get(2).getId());
        assertEquals(instance01.getId(), instances.get(1).getId());
        assertEquals(instance03.getId(), instances.get(0).getId());

        assertEquals(Integer.valueOf(0), instances.get(2).getOrder());
        assertEquals(Integer.valueOf(1), instances.get(1).getOrder());
        assertEquals(Integer.valueOf(2), instances.get(0).getOrder());

        // Delete instance
        statisticalOperationsServiceFacade.deleteInstance(getServiceContextAdministrador(), instance02.getId());

        // Check for deleted instance
        try {
            statisticalOperationsServiceFacade.findInstanceById(getServiceContextAdministrador(), instance01.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INSTANCE_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check order
        instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextAdministrador(), operationDto.getId());
        assertEquals(instance01.getId(), instances.get(1).getId());
        assertEquals(instance03.getId(), instances.get(0).getId());

        assertEquals(Integer.valueOf(0), instances.get(1).getOrder());
        assertEquals(Integer.valueOf(1), instances.get(0).getOrder());

    }

    @Test
    public void testCheckCurrentInstances() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Create instance 01
        InstanceDto instance01 = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        assertNotNull(instance01);
        statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextAdministrador(), instance01.getId());
        statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContextAdministrador(), instance01.getId());

        // Create instance 02
        InstanceDto instance02 = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        assertNotNull(instance02);

        // Check initial order
        List<InstanceBaseDto> instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextAdministrador(), operationDto.getId());
        assertEquals(instance02.getId(), instances.get(0).getId());
        assertEquals(instance01.getId(), instances.get(1).getId());

        assertEquals(Integer.valueOf(1), instances.get(0).getOrder());
        assertEquals(Integer.valueOf(0), instances.get(1).getOrder());

        // Check current instances
        operationDto = statisticalOperationsServiceFacade.findOperationById(getServiceContextAdministrador(), operationDto.getId());
        assertEquals(instance01.getId(), operationDto.getCurrentInstance().getId());
        assertEquals(null, operationDto.getCurrentInternalInstance());

        // Reorder instances
        List<Long> newOrder = new ArrayList<Long>();
        newOrder.add(instance02.getId());
        newOrder.add(instance01.getId());
        statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContextAdministrador(), operationDto.getId(), newOrder);

        // Check order
        instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextAdministrador(), operationDto.getId());
        assertEquals(instance01.getId(), instances.get(0).getId());
        assertEquals(instance02.getId(), instances.get(1).getId());

        assertEquals(Integer.valueOf(0), instances.get(1).getOrder());
        assertEquals(Integer.valueOf(1), instances.get(0).getOrder());

        // Check current instances
        operationDto = statisticalOperationsServiceFacade.findOperationById(getServiceContextAdministrador(), operationDto.getId());
        assertEquals(instance01.getId(), operationDto.getCurrentInstance().getId());
        assertEquals(null, operationDto.getCurrentInternalInstance());

        // Create instance 03
        InstanceDto instance03 = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        assertNotNull(instance03);
        statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextAdministrador(), instance03.getId());

        // Check order
        instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextAdministrador(), operationDto.getId());
        assertEquals(instance02.getId(), instances.get(2).getId());
        assertEquals(instance01.getId(), instances.get(1).getId());
        assertEquals(instance03.getId(), instances.get(0).getId());

        assertEquals(Integer.valueOf(0), instances.get(2).getOrder());
        assertEquals(Integer.valueOf(1), instances.get(1).getOrder());
        assertEquals(Integer.valueOf(2), instances.get(0).getOrder());

        // Check current instances
        operationDto = statisticalOperationsServiceFacade.findOperationById(getServiceContextAdministrador(), operationDto.getId());
        assertEquals(instance01.getId(), operationDto.getCurrentInstance().getId());
        assertEquals(instance03.getId(), operationDto.getCurrentInternalInstance().getId());

        // Delete instance
        statisticalOperationsServiceFacade.deleteInstance(getServiceContextAdministrador(), instance02.getId());

        // Publish externally instance03
        statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContextAdministrador(), instance03.getId());

        // Check for deleted instance
        try {
            statisticalOperationsServiceFacade.findInstanceById(getServiceContextAdministrador(), instance01.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INSTANCE_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check order
        instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContextAdministrador(), operationDto.getId());
        assertEquals(instance01.getId(), instances.get(1).getId());
        assertEquals(instance03.getId(), instances.get(0).getId());

        assertEquals(Integer.valueOf(0), instances.get(1).getOrder());
        assertEquals(Integer.valueOf(1), instances.get(0).getOrder());

        // Check current instances
        operationDto = statisticalOperationsServiceFacade.findOperationById(getServiceContextAdministrador(), operationDto.getId());
        assertEquals(instance03.getId(), operationDto.getCurrentInstance().getId());
        assertEquals(null, operationDto.getCurrentInternalInstance());

    }

    // @Test
    // public void testDeleteInstanceById() throws MetamacException {
    // initializeData();
    //
    // List<InstanceDto> instances = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador());
    //
    // int numberInstances = instances.size();
    // InstanceDto instanceDto = instances.get(0);
    //
    // statisticalOperationsServiceFacade.deleteInstance(getServiceContextAdministrador(), instanceDto.getId());
    // assertTrue(statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size() == (numberInstances - 1));
    //
    // try {
    // statisticalOperationsServiceFacade.findInstanceById(getServiceContextAdministrador(), instanceDto.getId());
    // } catch (MetamacException e) {
    // assertEquals(ServiceExceptionType.SERVICE_INSTANCE_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
    // }
    //
    // }

    @Test
    public void testDeleteInstanceInternallyPublished() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        int instancesBeforeCreateInstance = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        assertNotNull(instanceDto);

        int instancesBeforeDelete = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();
        assertEquals(instancesBeforeDelete, instancesBeforeCreateInstance + 1);

        // Set PUBLISH_INTERNALLY procStatus
        instanceDto = statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextAdministrador(), instanceDto.getId());

        // Delete instance with an incorrect procStatus
        try {
            statisticalOperationsServiceFacade.deleteInstance(getServiceContextAdministrador(), instanceDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }

        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();
        assertEquals(instancesBeforeDelete, instancesAfter);
    }

    @Test
    public void testDeleteInstanceExternallyPublished() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());
        assertNotNull(instanceDto);

        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();

        // Set PUBLISH_INTERNALLY procStatus
        instanceDto = statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextAdministrador(), instanceDto.getId());
        instanceDto = statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContextAdministrador(), instanceDto.getId());

        // Delete instance with an incorrect procStatus
        try {
            statisticalOperationsServiceFacade.deleteInstance(getServiceContextAdministrador(), instanceDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }

        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();
        assertEquals(instancesBefore, instancesAfter);
    }

    @Test
    public void testFindAllInstances() throws MetamacException {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto());
        assertNotNull(instanceDto);

        List<InstanceBaseDto> instances = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador());
        assertTrue(!instances.isEmpty());
    }

    @Test
    public void testFindInstanceByCondition() throws MetamacException {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto());
        statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto());
        statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto());

        MetamacCriteria criteria = new MetamacCriteria();
        MetamacCriteriaResult<InstanceBaseDto> result = statisticalOperationsServiceFacade.findInstanceByCondition(getServiceContextAdministrador(), criteria);
        assertTrue(result.getResults().size() >= 2);
    }

    @Test
    public void testFindInstanceByConditionPaginated() throws MetamacException {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto());
        statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto());
        statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto());

        MetamacCriteria criteria = new MetamacCriteria();

        MetamacCriteriaPaginator paginator = new MetamacCriteriaPaginator();
        paginator.setCountTotalResults(Boolean.TRUE);
        paginator.setMaximumResultSize(Integer.valueOf(2));
        criteria.setPaginator(paginator);

        {
            // Page 1
            criteria.getPaginator().setFirstResult(Integer.valueOf(0));

            MetamacCriteriaResult<InstanceBaseDto> result = statisticalOperationsServiceFacade.findInstanceByCondition(getServiceContextAdministrador(), criteria);
            assertEquals(2, result.getResults().size());
            assertTrue(result.getPaginatorResult().getTotalResults() >= 3);
            assertEquals(Integer.valueOf(0), result.getPaginatorResult().getFirstResult());
        }

        {
            // Page 2
            criteria.getPaginator().setFirstResult(Integer.valueOf(2));
            MetamacCriteriaResult<InstanceBaseDto> result = statisticalOperationsServiceFacade.findInstanceByCondition(getServiceContextAdministrador(), criteria);
            assertTrue(result.getPaginatorResult().getTotalResults() >= 1);
            assertEquals(Integer.valueOf(2), result.getPaginatorResult().getFirstResult());

        }
    }

    @Test
    public void testFindInstanceById() throws MetamacException {
        // Create instance
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());

        InstanceDto instanceRetrieved = statisticalOperationsServiceFacade.findInstanceById(getServiceContextAdministrador(), instanceDto.getId());

        assertNotNull(instanceRetrieved);
        assertTrue(instanceDto.getId().equals(instanceRetrieved.getId()));
    }

    @Test
    public void testPublishInternallyInstance() throws MetamacException {
        // Service doesn't check if the associated operation is PUBLISHED_INTERNALLY because it's a requirement for create it.

        // Create a PUBLISH_INTERNALLY Operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        assertTrue(ProcStatusEnum.PUBLISH_INTERNALLY.equals(operationDto.getProcStatus()));

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());

        int instancesBeforePublish = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();
        int operationsBeforePublish = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();

        // Publish instance
        instanceDto = statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextAdministrador(), instanceDto.getId());

        // Validations
        assertNotNull(operationDto.getInternalInventoryDate());
        assertTrue(ProcStatusEnum.PUBLISH_INTERNALLY.equals(instanceDto.getProcStatus()));

        // Check number of operations and instances
        int instancesAfterPublish = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();
        int operationsAfterPublish = statisticalOperationsServiceFacade.findAllOperations(getServiceContextAdministrador()).size();
        assertEquals(instancesBeforePublish, instancesAfterPublish);
        assertEquals(operationsBeforePublish, operationsAfterPublish);
    }

    @Test
    public void testPublishExternallyInstance() throws MetamacException {
        // Create a PUBLISH_EXTERNALLY Operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContextAdministrador(), operationDto.getId());
        assertTrue(ProcStatusEnum.PUBLISH_EXTERNALLY.equals(operationDto.getProcStatus()));

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());

        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();

        // Publish instance
        instanceDto = statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextAdministrador(), instanceDto.getId());
        instanceDto = statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContextAdministrador(), instanceDto.getId());

        // Validations
        assertNotNull(operationDto.getInternalInventoryDate());
        assertNotNull(operationDto.getInventoryDate());
        assertTrue(ProcStatusEnum.PUBLISH_EXTERNALLY.equals(instanceDto.getProcStatus()));

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();
        assertEquals(instancesBefore, instancesAfter);

        // Check that instance can't be internally published
        try {
            statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContextAdministrador(), instanceDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testFindOperationForInstance() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationDto.getId());

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationDto.getId(), createInstanceDto());

        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();

        OperationBaseDto operationRetrieved = statisticalOperationsServiceFacade.findOperationForInstance(getServiceContextAdministrador(), instanceDto.getId());

        assertEquals(operationRetrieved.getId(), operationDto.getId());

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();
        assertEquals(instancesBefore, instancesAfter);
    }

    @Test
    public void testFindInstanceBaseById() throws MetamacException {
        // Create instance
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContextAdministrador(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContextAdministrador(), operationId);

        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContextAdministrador(), operationId, createInstanceDto());

        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();

        InstanceBaseDto instanceRetrieved = statisticalOperationsServiceFacade.findInstanceBaseById(getServiceContextAdministrador(), instanceDto.getId());

        assertNotNull(instanceRetrieved);
        assertTrue(instanceDto.getId().equals(instanceRetrieved.getId()));

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContextAdministrador()).size();
        assertEquals(instancesBefore, instancesAfter);

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

    private FamilyDto createFamilyDtoForUpdate() {
        FamilyDto familyDto = new FamilyDto();

        // Auditable information
        familyDto.setCreatedBy("Junit");
        familyDto.setCreatedDate(new Date());

        // Identifier
        familyDto.setCode("PRUEBA01" + RandomStringUtils.random(50, true, true));

        // TITLE
        InternationalStringDto title = new InternationalStringDto();
        LocalisedStringDto title_es = new LocalisedStringDto();
        title_es.setLabel("NO VISIBLE Título en español de familia");
        title_es.setLocale("es");
        LocalisedStringDto title_en = new LocalisedStringDto();
        title_en.setLabel("NO VISIBLE Título en inglés de familia");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        familyDto.setTitle(title);

        // DESCRIPTION
        InternationalStringDto description = new InternationalStringDto();
        LocalisedStringDto description_es = new LocalisedStringDto();
        description_es.setLabel("NO VISIBLE Descripción en español de familia");
        description_es.setLocale("es");
        LocalisedStringDto description_en = new LocalisedStringDto();
        description_en.setLabel("NO VISIBLE Descripción en inglés de familia");
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

        // ACRONYM
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
        ExternalItemBtDto subjectArea = new ExternalItemBtDto();
        subjectArea.setCodeId("PRUEBA");
        subjectArea.setType(TypeExternalArtefactsEnum.CATEGORY);
        subjectArea.setUriInt("uri:internal:todo");
        operationDto.setSubjectArea(subjectArea);

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
        ExternalItemBtDto producer01 = new ExternalItemBtDto();
        producer01.setCodeId("ISTAC");
        producer01.setType(TypeExternalArtefactsEnum.AGENCY);
        producer01.setUriInt("uri:interna:todo");
        operationDto.addProducer(producer01);

        ExternalItemBtDto producer02 = new ExternalItemBtDto();
        producer02.setCodeId("INE");
        producer02.setType(TypeExternalArtefactsEnum.AGENCY);
        producer02.setUriInt("uri:interna:todo");
        operationDto.addProducer(producer02);

        // REGIONAL_RESPONSIBLE
        ExternalItemBtDto regionalResponsible01 = new ExternalItemBtDto();
        regionalResponsible01.setCodeId("ISTAC");
        regionalResponsible01.setType(TypeExternalArtefactsEnum.AGENCY);
        regionalResponsible01.setUriInt("uri:interna:todo");
        operationDto.addRegionalResponsible(regionalResponsible01);

        // PUBLISHER
        ExternalItemBtDto publisher01 = new ExternalItemBtDto();
        publisher01.setCodeId("ISTAC");
        publisher01.setType(TypeExternalArtefactsEnum.AGENCY);
        publisher01.setUriInt("uri:interna:todo");
        operationDto.addPublisher(publisher01);

        // COMMON_METADATA
        ExternalItemBtDto commonMetadata = new ExternalItemBtDto();
        commonMetadata.setCodeId("ISTAC");
        commonMetadata.setType(TypeExternalArtefactsEnum.AGENCY);
        commonMetadata.setUriInt("uri:interna:todo");
        operationDto.setCommonMetadata(commonMetadata);

        return operationDto;
    }

    private OperationDto createOperationDtoWithProducer() throws MetamacException {
        OperationDto operationDto = createOperationDto();

        // PRODUCER
        ExternalItemBtDto producer01 = new ExternalItemBtDto();
        producer01.setCodeId("ISTAC_FOR_REMOVE");
        producer01.setType(TypeExternalArtefactsEnum.AGENCY);
        producer01.setUriInt("uri:interna:todo");
        operationDto.addProducer(producer01);

        ExternalItemBtDto producer02 = new ExternalItemBtDto();
        producer02.setCodeId("INE_FOR_REMOVE");
        producer02.setType(TypeExternalArtefactsEnum.AGENCY);
        producer02.setUriInt("uri:interna:todo");
        operationDto.addProducer(producer02);

        return operationDto;
    }

    private OperationDto createOperationDtoWithOfficialityType() throws MetamacException {
        OperationDto operationDto = createOperationDto();

        // OFFICIALITY TYPE
        operationDto.setOfficialityType(statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContextAdministrador(), Long.valueOf(1)));

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

    private InstanceDto createInstanceDtoWithGeographicGranularity() throws MetamacException {
        InstanceDto instanceDto = createInstanceDto();

        // GEOGRAPHIC_GRANULARITY
        ExternalItemBtDto geographicGranularity = new ExternalItemBtDto();
        geographicGranularity.setCodeId("GEOGRAPHIC_GRANULARITY");
        geographicGranularity.setType(TypeExternalArtefactsEnum.CONCEPT);
        geographicGranularity.setUriInt("uri:interna:todo");
        instanceDto.setGeographicGranularity(geographicGranularity);

        return instanceDto;

    }

    private InstanceDto createIncompleteInstanceDto() {
        InstanceDto instanceDto = new InstanceDto();

        // Auditable information
        instanceDto.setCreatedBy("Junit");
        instanceDto.setCreatedDate(new Date());

        // Identifier
        instanceDto.setCode("PRUEBA01" + RandomStringUtils.random(50, true, true));

        return instanceDto;
    }

}
