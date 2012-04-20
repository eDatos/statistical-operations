package org.siemac.metamac.statistical.operations.core.serviceapi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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
        List<SurveyTypeDto> surveyTypesList = statisticalOperationsServiceFacade.findAllSurveyTypes(getServiceContext());
        assertTrue(!surveyTypesList.isEmpty());
    }

    @Test
    public void testFindSurveyTypeById() throws MetamacException {
        SurveyTypeDto surveyTypeDto = statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContext(), Long.valueOf(1));
        assertNotNull(surveyTypeDto);
    }

    /**************************************************************************
     * Instance Type
     **************************************************************************/

    @Test
    public void testFindAllInstanceTypes() throws MetamacException {
        List<InstanceTypeDto> instanceTypesList = statisticalOperationsServiceFacade.findAllInstanceTypes(getServiceContext());
        assertTrue(!instanceTypesList.isEmpty());
    }

    @Test
    public void testFindInstanceTypeById() throws MetamacException {
        InstanceTypeDto instanceTypeDto = statisticalOperationsServiceFacade.findInstanceTypeById(getServiceContext(), Long.valueOf(1));
        assertNotNull(instanceTypeDto);
    }

    /**************************************************************************
     * Survey Source
     **************************************************************************/

    @Test
    public void testFindAllSurveySources() throws MetamacException {
        List<SurveySourceDto> surveySourcesList = statisticalOperationsServiceFacade.findAllSurveySources(getServiceContext());
        assertTrue(!surveySourcesList.isEmpty());
    }

    @Test
    public void testFindSurveySourceById() throws MetamacException {
        SurveySourceDto surveySourceDto = statisticalOperationsServiceFacade.findSurveySourceById(getServiceContext(), Long.valueOf(1));
        assertNotNull(surveySourceDto);
    }

    /**************************************************************************
     * Officiality Type
     **************************************************************************/

    @Test
    public void testFindAllOfficialityTypes() throws MetamacException {
        List<OfficialityTypeDto> officialityTypesList = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContext());
        assertTrue(!officialityTypesList.isEmpty());
    }

    @Test
    public void testFindOfficialityTypeById() throws MetamacException {
        OfficialityTypeDto officialityTypeDto = statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContext(), Long.valueOf(1));
        assertNotNull(officialityTypeDto);
    }

    /**************************************************************************
     * Coll Methods
     **************************************************************************/

    @Test
    public void testFindAllCollMethods() throws Exception {
        List<CollMethodDto> collMethodsList = statisticalOperationsServiceFacade.findAllCollMethods(getServiceContext());
        assertTrue(!collMethodsList.isEmpty());

    }

    @Test
    public void testFindCollMethodById() throws Exception {
        CollMethodDto collMethodDto = statisticalOperationsServiceFacade.findCollMethodById(getServiceContext(), Long.valueOf(1));
        assertNotNull(collMethodDto);

    }

    /**************************************************************************
     * Costs
     **************************************************************************/

    @Test
    public void testFindAllCosts() throws Exception {
        List<CostDto> costsList = statisticalOperationsServiceFacade.findAllCosts(getServiceContext());
        assertTrue(!costsList.isEmpty());

    }

    @Test
    public void testFindCostById() throws Exception {
        CostDto costDto = statisticalOperationsServiceFacade.findCostById(getServiceContext(), Long.valueOf(1));
        assertNotNull(costDto);

    }

    /**************************************************************************
     * Family
     **************************************************************************/

    @Test
    public void testCreateFamily() throws MetamacException {
        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

        FamilyDto familyDto = createFamilyDto();

        familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), familyDto);
        assertNotNull(familyDto);

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertEquals(familiesBefore + 1, familiesAfter);
    }

    @Test
    public void testCreateFamilyDuplicatedCode() throws MetamacException {
        FamilyDto persistedFamilyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());

        FamilyDto familyDto = createFamilyDto();
        familyDto.setCode(persistedFamilyDto.getCode());

        try {
            statisticalOperationsServiceFacade.createFamily(getServiceContext(), familyDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.FAMILY_ALREADY_EXIST_CODE_DUPLICATED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testUpdateFamily() throws MetamacException {
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDtoForUpdate());

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

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

        familyDto = statisticalOperationsServiceFacade.updateFamily(getServiceContext(), familyDto);

        assertNotNull(familyDto);

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertEquals(familiesBefore, familiesAfter);
    }

    @Test
    public void testUpdateFamilyWithoutDescription() throws MetamacException {
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDtoForUpdate());

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

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

        familyDto = statisticalOperationsServiceFacade.updateFamily(getServiceContext(), familyDto);

        assertNotNull(familyDto);

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertEquals(familiesBefore, familiesAfter);
    }

    @Test
    public void testUpdateFamilyWithoutLocalisedString() throws MetamacException {
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDtoForUpdate());

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

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

        familyDto = statisticalOperationsServiceFacade.updateFamily(getServiceContext(), familyDto);

        assertNotNull(familyDto);

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertEquals(familiesBefore, familiesAfter);
    }

    @Test
    public void testUpdateFamilyWithOperations() throws MetamacException {
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDtoForUpdate());

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

        OperationDto operation01 = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContext(), familyDto.getId(), operation01.getId());

        OperationDto operation02 = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContext(), familyDto.getId(), operation02.getId());

        // Check operations for family
        List<OperationBaseDto> operationsForFamilyBefore = statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContext(), familyDto.getId());
        assertEquals(2, operationsForFamilyBefore.size());

        // Get family after modified
        familyDto = statisticalOperationsServiceFacade.findFamilyById(getServiceContext(), familyDto.getId());

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

        familyDto = statisticalOperationsServiceFacade.updateFamily(getServiceContext(), familyDto);

        assertNotNull(familyDto);

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertEquals(familiesBefore, familiesAfter);

        // Check operations for family
        List<OperationBaseDto> operationsForFamilyAfter = statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContext(), familyDto.getId());
        assertEquals(operationsForFamilyBefore.size(), operationsForFamilyAfter.size());
    }

    @Test
    public void testDeleteFamily() throws MetamacException {
        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDtoForUpdate());
        assertNotNull(familyDto);
        int numberFamilies = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

        // Remove family
        statisticalOperationsServiceFacade.deleteFamily(getServiceContext(), familyDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size() == (numberFamilies - 1));

        try {
            statisticalOperationsServiceFacade.findFamilyById(getServiceContext(), familyDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.FAMILY_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testDeleteFamilyWithOperations() throws MetamacException {
        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDtoForUpdate());
        assertNotNull(familyDto);
        int numberFamilies = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

        // Create operations
        OperationDto operation01 = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContext(), familyDto.getId(), operation01.getId());

        OperationDto operation02 = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContext(), familyDto.getId(), operation02.getId());

        // Check number of operations before delete
        int operationsBeforeDelete = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        // Remove family
        statisticalOperationsServiceFacade.deleteFamily(getServiceContext(), familyDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size() == (numberFamilies - 1));

        // Check for deleted family
        try {
            statisticalOperationsServiceFacade.findFamilyById(getServiceContext(), familyDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.FAMILY_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check number of operations after delete
        int operationsAfterDelete = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(operationsBeforeDelete, operationsAfterDelete);
    }

    @Test
    public void testDeleteFamilyInternallyPublished() throws MetamacException {
        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        assertNotNull(familyDto);

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

        // Set PUBLISH_INTERNALLY procStatus
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContext(), familyDto.getId(), operationDto.getId());

        familyDto = statisticalOperationsServiceFacade.findFamilyById(getServiceContext(), familyDto.getId());
        familyDto = statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContext(), familyDto.getId());

        // Delete family with an incorrect procStatus
        try {
            statisticalOperationsServiceFacade.deleteFamily(getServiceContext(), familyDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }
        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertEquals(familiesBefore, familiesAfter);

    }

    @Test
    public void testDeleteFamilyExternallyPublished() throws MetamacException {
        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        assertNotNull(familyDto);

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

        // Set operation procStatus
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());
        operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContext(), operationDto.getId());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContext(), familyDto.getId(), operationDto.getId());

        // Set PUBLISH_EXTERNALLY procStatus
        familyDto = statisticalOperationsServiceFacade.findFamilyById(getServiceContext(), familyDto.getId());
        familyDto = statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContext(), familyDto.getId());
        familyDto = statisticalOperationsServiceFacade.publishExternallyFamily(getServiceContext(), familyDto.getId());

        // Delete family with an incorrect procStatus
        try {
            statisticalOperationsServiceFacade.deleteFamily(getServiceContext(), familyDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertEquals(familiesBefore, familiesAfter);
    }

    @Test
    public void testFindAllFamilies() throws MetamacException {
        statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());

        List<FamilyBaseDto> families = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext());
        assertTrue(!families.isEmpty());

    }

    @Test
    public void testFindFamilyByCondition() throws MetamacException {
        statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());

        MetamacCriteria criteria = new MetamacCriteria();
        MetamacCriteriaResult<FamilyBaseDto> result = statisticalOperationsServiceFacade.findFamilyByCondition(getServiceContext(), criteria);
        assertTrue(result.getResults().size() >= 2);
    }

    @Test
    public void testFindFamilyByConditionPaginated() throws MetamacException {
        statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());

        MetamacCriteria criteria = new MetamacCriteria();

        MetamacCriteriaPaginator paginator = new MetamacCriteriaPaginator();
        paginator.setCountTotalResults(Boolean.TRUE);
        paginator.setMaximumResultSize(Integer.valueOf(2));
        criteria.setPaginator(paginator);

        {
            // Page 1
            criteria.getPaginator().setFirstResult(Integer.valueOf(0));

            MetamacCriteriaResult<FamilyBaseDto> result = statisticalOperationsServiceFacade.findFamilyByCondition(getServiceContext(), criteria);
            assertEquals(2, result.getResults().size());
            assertTrue(result.getPaginatorResult().getTotalResults() >= 3);
            assertEquals(Integer.valueOf(0), result.getPaginatorResult().getFirstResult());
        }

        {
            // Page 2
            criteria.getPaginator().setFirstResult(Integer.valueOf(2));
            MetamacCriteriaResult<FamilyBaseDto> result = statisticalOperationsServiceFacade.findFamilyByCondition(getServiceContext(), criteria);
            assertTrue(result.getPaginatorResult().getTotalResults() >= 1);
            assertEquals(Integer.valueOf(2), result.getPaginatorResult().getFirstResult());

        }
    }

    @Test
    public void testFindFamilyById() throws MetamacException {
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());

        FamilyDto familyRetrieved = statisticalOperationsServiceFacade.findFamilyById(getServiceContext(), familyDto.getId());

        assertNotNull(familyRetrieved);
        assertTrue(familyDto.getId().equals(familyRetrieved.getId()));
    }

    @Test
    public void testPublishInternallyFamilyError() throws MetamacException {
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContext(), familyDto.getId(), operationDto.getId());

        familyDto = statisticalOperationsServiceFacade.findFamilyById(getServiceContext(), familyDto.getId());

        try {
            familyDto = statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContext(), familyDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.FAMILY_WITHOUT_PUBLISHED_INTERNALLY_OPERATIONS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }

        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertTrue(familiesBefore == familiesAfter);
    }

    @Test
    public void testPublishInternallyFamily() throws MetamacException {
        // Create operation with ProcStatus PUBLISH_EXTERNALLY
        OperationDto operationDto = createOperationDtoForInternalPublishing();
        operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), operationDto);
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());
        assertNotNull(operationDto.getInternalInventoryDate());
        assertEquals(ProcStatusEnum.PUBLISH_INTERNALLY, operationDto.getProcStatus());

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

        // Associate
        List<OperationBaseDto> operationsForFamily = statisticalOperationsServiceFacade.addOperationForFamily(getServiceContext(), familyDto.getId(), operationDto.getId());
        assertNotNull(operationsForFamily);

        // Reload family
        familyDto = statisticalOperationsServiceFacade.findFamilyById(getServiceContext(), familyDto.getId());

        // Publish family
        familyDto = statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContext(), familyDto.getId());

        // Validations
        assertNotNull(familyDto);
        assertNotNull(familyDto.getInternalInventoryDate());
        assertTrue(ProcStatusEnum.PUBLISH_INTERNALLY.equals(familyDto.getProcStatus()));
        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertTrue(familiesBefore == familiesAfter);
    }

    @Test
    public void testPublishExternallyFamily() throws MetamacException {

        // Create operation with ProcStatus PUBLISH_EXTERNALLY
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());
        operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContext(), operationDto.getId());
        assertNotNull(operationDto.getInternalInventoryDate());
        assertNotNull(operationDto.getInventoryDate());
        assertEquals(ProcStatusEnum.PUBLISH_EXTERNALLY, operationDto.getProcStatus());

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

        // Associate
        List<OperationBaseDto> operationsForFamily = statisticalOperationsServiceFacade.addOperationForFamily(getServiceContext(), familyDto.getId(), operationDto.getId());
        assertNotNull(operationsForFamily);

        // Reload family
        familyDto = statisticalOperationsServiceFacade.findFamilyById(getServiceContext(), familyDto.getId());

        // Publish family
        familyDto = statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContext(), familyDto.getId());
        familyDto = statisticalOperationsServiceFacade.publishExternallyFamily(getServiceContext(), familyDto.getId());
        assertTrue(ProcStatusEnum.PUBLISH_EXTERNALLY.equals(familyDto.getProcStatus()));

        // Validations
        assertNotNull(familyDto);
        assertNotNull(familyDto.getInternalInventoryDate());
        assertNotNull(familyDto.getInventoryDate());
        assertTrue(ProcStatusEnum.PUBLISH_EXTERNALLY.equals(familyDto.getProcStatus()));
        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertEquals(familiesBefore, familiesAfter);

        // Check that family can't be internally published
        try {
            statisticalOperationsServiceFacade.publishInternallyFamily(getServiceContext(), familyDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testFindOperationsForFamily() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        assertNotNull(operationDto);

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        assertNotNull(familyDto);

        // Associate
        List<OperationBaseDto> operationsForFamily = statisticalOperationsServiceFacade.addOperationForFamily(getServiceContext(), familyDto.getId(), operationDto.getId());

        assertNotNull(operationsForFamily);
    }

    @Test
    public void testAddOperationForFamily() throws MetamacException {

        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        assertNotNull(operationDto);

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        assertNotNull(familyDto);

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

        // Associate
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContext(), familyDto.getId(), operationDto.getId());

        assertNotNull(statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContext(), familyDto.getId()));

        // Check number of families
        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertEquals(familiesBefore, familiesAfter);
    }

    @Test
    public void testRemoveOperationForFamily() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        assertNotNull(operationDto);

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        assertNotNull(familyDto);

        int familiesBefore = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

        // Associate
        statisticalOperationsServiceFacade.addOperationForFamily(getServiceContext(), familyDto.getId(), operationDto.getId());

        assertNotNull(statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContext(), familyDto.getId()));

        // Remove
        statisticalOperationsServiceFacade.removeOperationForFamily(getServiceContext(), familyDto.getId(), operationDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findOperationsForFamily(getServiceContext(), familyDto.getId()).isEmpty());

        // Check number of families
        int familiesAfter = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertEquals(familiesBefore, familiesAfter);
    }

    /**************************************************************************
     * Operation
     **************************************************************************/

    @Test
    public void testCreateOperation() throws MetamacException {

        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        assertNotNull(operationDto);

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(1, operationsAfter);
    }

    @Test
    public void testCreateOperationDuplicatedCode() throws MetamacException {
        OperationDto persistedOperationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());

        OperationDto operationDto = createOperationDto();
        operationDto.setCode(persistedOperationDto.getCode());

        try {
            statisticalOperationsServiceFacade.createOperation(getServiceContext(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.OPERATION_ALREADY_EXIST_CODE_DUPLICATED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testUpdateOperation() throws Exception {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        assertNotNull(operationDto);

        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

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

        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContext(), operationDto);

        assertNotNull(operationDto);

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testUpdateOperationWithDescriptionWithoutLocales() throws Exception {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        assertNotNull(operationDto);

        // DESCRIPTION
        InternationalStringDto description = new InternationalStringDto();
        operationDto.setDescription(description);

        try {
            statisticalOperationsServiceFacade.updateOperation(getServiceContext(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.METADATA_REQUIRED.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(ServiceExceptionParameters.OPERATION_DESCRIPTION, e.getExceptionItems().get(0).getMessageParameters()[0]);
        }

    }

    @Test
    public void testUpdateOperationStatus() throws Exception {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        assertNotNull(operationDto);

        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        // STATUS
        operationDto.setStatus(StatusEnum.DESIGN);

        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContext(), operationDto);

        assertNotNull(operationDto);
        assertEquals(StatusEnum.DESIGN, operationDto.getStatus());

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testUpdateOperationWithExternalItems() throws Exception {
        // Create operation
        int externalItemsBefore = externalItemRepository.findAll().size();

        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoWithProducer());

        int externalItemsAfter = externalItemRepository.findAll().size();
        assertEquals(externalItemsBefore + 2, externalItemsAfter);

        // ADD PRODUCER
        externalItemsBefore = externalItemRepository.findAll().size();

        ExternalItemBtDto producerUpdated = new ExternalItemBtDto();
        producerUpdated.setCodeId("ISTAC_MOD");
        producerUpdated.setType(TypeExternalArtefactsEnum.AGENCY);
        producerUpdated.setUriInt("uri:internal_mod:todo");
        operationDto.addProducer(producerUpdated);
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContext(), operationDto);

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
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContext(), operationDto);

        externalItemsAfter = externalItemRepository.findAll().size();
        assertEquals(externalItemsBefore - 1, externalItemsAfter);

        // PRODUCER: REMOVE ALL
        externalItemsBefore = externalItemRepository.findAll().size();

        operationDto.removeAllProducer();
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContext(), operationDto);

        externalItemsAfter = externalItemRepository.findAll().size();
        assertEquals(externalItemsBefore - 2, externalItemsAfter);

    }

    @Test
    public void testUpdateOperationWithoutExternalItemsPreviuslySave() throws Exception {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());

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

        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContext(), operationDto);

        int externalItemsAfter = externalItemRepository.findAll().size();
        assertEquals(externalItemsBefore + 2, externalItemsAfter);

        // CLEAR REGIONAL CONTRIBUTOR
        externalItemsBefore = externalItemRepository.findAll().size();

        operationDto.getRegionalContributor().clear();
        operationDto.getRegionalContributor().addAll(new ArrayList<ExternalItemBtDto>());

        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContext(), operationDto);

        externalItemsAfter = externalItemRepository.findAll().size();
        assertEquals(externalItemsBefore - 2, externalItemsAfter);

    }

    @Test
    public void testCreateOperationWithIncorrectReleaseCalendarAccess() throws MetamacException {
        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        OperationDto operationDto = createOperationDto();
        operationDto.setReleaseCalendarAccess("INCORRECT URL");

        try {
            operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), operationDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_URL.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testUpdateOperationWithList() throws Exception {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoWithOfficialityType());
        assertNotNull(operationDto);
        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        // MODIFY OFFICIALITY TYPE
        int officialityTypesBefore = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContext()).size();

        operationDto.setOfficialityType(statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContext(), Long.valueOf(2)));
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContext(), operationDto);

        int officialityTypesAfter = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContext()).size();
        assertEquals(officialityTypesBefore, officialityTypesAfter);

        // REMOVE OFFICIALITY TYPE
        officialityTypesBefore = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContext()).size();

        operationDto.setOfficialityType(null);
        operationDto = statisticalOperationsServiceFacade.updateOperation(getServiceContext(), operationDto);

        officialityTypesAfter = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContext()).size();
        assertEquals(officialityTypesBefore, officialityTypesAfter);

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testDeleteOperationWithList() throws Exception {
        // Create operation with officiality type
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoWithOfficialityType());
        assertNotNull(operationDto);
        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        // Delete operation
        int officialityTypesBefore = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContext()).size();

        statisticalOperationsServiceFacade.deleteOperation(getServiceContext(), operationDto.getId());

        int officialityTypesAfter = statisticalOperationsServiceFacade.findAllOfficialityTypes(getServiceContext()).size();
        assertEquals(officialityTypesBefore, officialityTypesAfter);

        // Retrieve deleted operation
        try {
            statisticalOperationsServiceFacade.findOperationById(getServiceContext(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.OPERATION_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(operationsBefore - 1, operationsAfter);
    }

    @Test
    public void testDeleteOperationWithProducer() throws Exception {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        assertNotNull(operationDto);
        int numberOperations = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        statisticalOperationsServiceFacade.deleteOperation(getServiceContext(), operationDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size() == (numberOperations - 1));

        try {
            statisticalOperationsServiceFacade.findOperationById(getServiceContext(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.OPERATION_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testDeleteOperation() throws MetamacException {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        assertNotNull(operationDto);
        int numberOperations = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        statisticalOperationsServiceFacade.deleteOperation(getServiceContext(), operationDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size() == (numberOperations - 1));

        try {
            statisticalOperationsServiceFacade.findOperationById(getServiceContext(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.OPERATION_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testDeleteOperationWithFamilies() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        assertNotNull(operationDto);
        int numberOperations = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        // Create families and associate
        FamilyDto family01 = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContext(), operationDto.getId(), family01.getId());

        FamilyDto family02 = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContext(), operationDto.getId(), family02.getId());

        // Count number of families before delete
        int familiesBeforeDelete = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();

        // Get operation
        operationDto = statisticalOperationsServiceFacade.findOperationById(getServiceContext(), operationDto.getId());

        // Delete Operation
        statisticalOperationsServiceFacade.deleteOperation(getServiceContext(), operationDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size() == (numberOperations - 1));

        // Check for deleted operation
        try {
            statisticalOperationsServiceFacade.findOperationById(getServiceContext(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.OPERATION_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check number of families before delete
        int familiesAfterDelete = statisticalOperationsServiceFacade.findAllFamilies(getServiceContext()).size();
        assertEquals(familiesBeforeDelete, familiesAfterDelete);
    }

    @Test
    public void testDeleteOperationInternallyPublished() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        assertNotNull(operationDto);

        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        // Set PUBLISH_INTERNALLY procStatus
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        // Delete operation with an incorrect procStatus
        try {
            statisticalOperationsServiceFacade.deleteOperation(getServiceContext(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }

        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testDeleteOperationExternallyPublished() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        assertNotNull(operationDto);

        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        // Set PUBLISH_INTERNALLY procStatus
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());
        operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContext(), operationDto.getId());

        // Delete operation with an incorrect procStatus
        try {
            statisticalOperationsServiceFacade.deleteOperation(getServiceContext(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }

        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testFindAllOperations() throws MetamacException {
        statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        List<OperationBaseDto> operations = statisticalOperationsServiceFacade.findAllOperations(getServiceContext());
        assertTrue(!operations.isEmpty());
    }

    @Test
    public void testFindOperationsByCondition() throws MetamacException {
        statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());

        MetamacCriteria criteria = new MetamacCriteria();
        MetamacCriteriaResult<OperationBaseDto> result = statisticalOperationsServiceFacade.findOperationsByCondition(getServiceContext(), criteria);
        assertTrue(result.getResults().size() >= 2);
    }

    @Test
    public void testFindOperationsByConditionPaginated() throws MetamacException {
        statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());

        MetamacCriteria criteria = new MetamacCriteria();

        MetamacCriteriaPaginator paginator = new MetamacCriteriaPaginator();
        paginator.setCountTotalResults(Boolean.TRUE);
        paginator.setMaximumResultSize(Integer.valueOf(2));
        criteria.setPaginator(paginator);

        {
            // Page 1
            criteria.getPaginator().setFirstResult(Integer.valueOf(0));

            MetamacCriteriaResult<OperationBaseDto> result = statisticalOperationsServiceFacade.findOperationsByCondition(getServiceContext(), criteria);
            assertEquals(2, result.getResults().size());
            assertTrue(result.getPaginatorResult().getTotalResults() >= 3);
            assertEquals(Integer.valueOf(0), result.getPaginatorResult().getFirstResult());
        }

        {
            // Page 2
            criteria.getPaginator().setFirstResult(Integer.valueOf(2));
            MetamacCriteriaResult<OperationBaseDto> result = statisticalOperationsServiceFacade.findOperationsByCondition(getServiceContext(), criteria);
            assertTrue(result.getPaginatorResult().getTotalResults() >= 1);
            assertEquals(Integer.valueOf(2), result.getPaginatorResult().getFirstResult());

        }
    }

    @Test
    public void testFindOperationById() throws MetamacException {
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());

        OperationDto operationRetrieved = statisticalOperationsServiceFacade.findOperationById(getServiceContext(), operationDto.getId());

        assertNotNull(operationRetrieved);
        assertTrue(operationDto.getId().equals(operationRetrieved.getId()));
    }

    @Test
    public void testPublishInternallyOperation() throws MetamacException {

        // Create and Publish operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        // Validations
        assertNotNull(operationDto.getInternalInventoryDate());
        assertTrue(ProcStatusEnum.PUBLISH_INTERNALLY.equals(operationDto.getProcStatus()));

        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    @Test
    public void testPublishExternallyOperation() throws MetamacException {

        // Create and Publish operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());
        operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContext(), operationDto.getId());

        // Validations
        assertNotNull(operationDto.getInternalInventoryDate());
        assertNotNull(operationDto.getInventoryDate());
        assertTrue(ProcStatusEnum.PUBLISH_EXTERNALLY.equals(operationDto.getProcStatus()));

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(operationsBefore, operationsAfter);

        // Check that operation can't be internally published
        try {
            statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testFindFamiliesForOperation() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        assertNotNull(operationDto);

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        assertNotNull(familyDto);

        // Associate
        List<FamilyBaseDto> familiesForOperation = statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContext(), operationDto.getId(), familyDto.getId());

        assertNotNull(familiesForOperation);

    }

    @Test
    public void testFindInstancesForOperation() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        assertNotNull(operationDto);

        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        assertNotNull(instanceDto);

        assertEquals(1, statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationDto.getId()).size());
    }

    // @Test
    // public void testFindInstancesForOperationId() throws MetamacException {
    // List<InstanceDto> instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), Long.valueOf(123));
    //
    // assertNotNull(instances);
    // }

    @Test
    public void testAddFamilyForOperation() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        assertNotNull(operationDto);

        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        assertNotNull(familyDto);

        // Associate
        statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContext(), operationDto.getId(), familyDto.getId());

        assertNotNull(statisticalOperationsServiceFacade.findFamiliesForOperation(getServiceContext(), operationDto.getId()));

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(operationsBefore, operationsAfter);

    }

    @Test
    public void testRemoveFamilyForOperation() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDto());
        assertNotNull(operationDto);

        int operationsBefore = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        // Create family
        FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(getServiceContext(), createFamilyDto());
        assertNotNull(familyDto);

        // Associate
        statisticalOperationsServiceFacade.addFamilyForOperation(getServiceContext(), operationDto.getId(), familyDto.getId());
        assertNotNull(statisticalOperationsServiceFacade.findFamiliesForOperation(getServiceContext(), operationDto.getId()));

        // Remove
        statisticalOperationsServiceFacade.removeFamilyForOperation(getServiceContext(), operationDto.getId(), familyDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findFamiliesForOperation(getServiceContext(), operationDto.getId()).isEmpty());

        // Check number of operations
        int operationsAfter = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(operationsBefore, operationsAfter);
    }

    /**************************************************************************
     * Instance
     **************************************************************************/

    @Test
    public void testCreateInstance() throws MetamacException, InterruptedException {
        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();

        // Create operation
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationId);

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationId, createInstanceDto());
        assertNotNull(instanceDto);

        // Ckeck number of instances in the operation
        List<InstanceBaseDto> operationInstances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationId);
        assertEquals(1, operationInstances.size());

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
        assertEquals(instancesBefore + 1, instancesAfter);

    }

    @Test
    public void testCreateInstanceDuplicatedCode() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        Long operationId = operationDto.getId();
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        // Create instance
        InstanceDto persistedIstanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationId, createInstanceDto());

        InstanceDto instanceDto = createInstanceDto();
        instanceDto.setCode(persistedIstanceDto.getCode());

        try {
            statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationId, instanceDto);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INSTANCE_ALREADY_EXIST_CODE_DUPLICATED.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testCreateInstanceError() throws MetamacException {
        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();

        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        // Create instance
        try {
            statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createIncompleteInstanceDto());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.METADATA_REQUIRED.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
        assertEquals(instancesBefore, instancesAfter);

    }

    @Test
    public void testUpdateInstance() throws Exception {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        assertNotNull(instanceDto);

        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();

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

        instanceDto = statisticalOperationsServiceFacade.updateInstance(getServiceContext(), instanceDto);

        assertNotNull(operationDto);

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
        assertEquals(instancesBefore, instancesAfter);

    }

    @Test
    public void testUpdateInstanceWithExternalItemBt() throws Exception {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDtoWithGeographicGranularity());
        assertNotNull(instanceDto);

        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();

        // GEOGRAPHIC_GRANULARITY
        instanceDto.setGeographicGranularity(null);

        // Save
        instanceDto = statisticalOperationsServiceFacade.updateInstance(getServiceContext(), instanceDto);

        // Validations
        assertNotNull(operationDto);
        assertNotNull(instanceDto);
        assertTrue(instanceDto.getGeographicGranularity() == null);

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
        assertEquals(instancesBefore, instancesAfter);

    }

    // @Test
    // public void testUpdateInstancesOrder() throws Exception {
    // initializeData();
    //
    // int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
    //
    // // Create operation
    // OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
    // operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto);
    //
    //
    // for (int i = 0; i < 3; i++) {
    // InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
    // assertNotNull(instanceDto);
    // }
    //
    // // Check number of instances
    // int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
    // assertEquals(instancesBefore + 3, instancesAfter);
    //
    // // Change order
    // List<InstanceDto> instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationDto.getId());
    //
    // List<Long> instancesIds = new ArrayList<Long>();
    // instancesIds.add(instances.get(2).getId());
    // instancesIds.add(instances.get(1).getId());
    // instancesIds.add(instances.get(0).getId());
    //
    // List<InstanceDto> orderedInstances = statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContext(), operationDto.getId(), instancesIds);
    //
    // // Check correct order
    // assertEquals(orderedInstances.get(2).getId(), instances.get(0).getId());
    // assertEquals(orderedInstances.get(1).getId(), instances.get(1).getId());
    // assertEquals(orderedInstances.get(0).getId(), instances.get(2).getId());
    //
    // // Check order number
    // assertEquals(orderedInstances.get(0).getOrder(), Integer.valueOf(0));
    // assertEquals(orderedInstances.get(1).getOrder(), Integer.valueOf(1));
    // assertEquals(orderedInstances.get(2).getOrder(), Integer.valueOf(2));
    //
    // // Check number of instances
    // assertEquals(orderedInstances.size(), statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationDto.getId()).size());
    // }

    @Test
    public void testUpdateInstancesOrder() throws Exception {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        InstanceDto instance01 = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        InstanceDto instance02 = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        InstanceDto instance03 = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        
        List<Long> instancesIds = new ArrayList<Long>();
        instancesIds.add(instance03.getId());
        instancesIds.add(instance02.getId());
        instancesIds.add(instance01.getId());

        List<InstanceBaseDto> orderedInstances = statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContext(), operationDto.getId(), instancesIds);

        // Check correct order
        assertEquals(orderedInstances.get(2).getId(), instance01.getId());
        assertEquals(orderedInstances.get(1).getId(), instance02.getId());
        assertEquals(orderedInstances.get(0).getId(), instance03.getId());

        // Check order number
        // TODO: Check this
//        assertEquals(orderedInstances.get(0).getOrder(), Integer.valueOf(2));
//        assertEquals(orderedInstances.get(1).getOrder(), Integer.valueOf(1));
//        assertEquals(orderedInstances.get(2).getOrder(), Integer.valueOf(0));

        // Check number of instances
        assertEquals(3, statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationDto.getId()).size());
    }

    @Test
    public void testDeleteInstance() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        assertNotNull(instanceDto);

        int numberInstances = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();

        statisticalOperationsServiceFacade.deleteInstance(getServiceContext(), instanceDto.getId());
        assertTrue(statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size() == (numberInstances - 1));

        try {
            statisticalOperationsServiceFacade.findInstanceById(getServiceContext(), instanceDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INSTANCE_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }

    }

    @Test
    public void testCheckInstancesOrderAfterRemoveInstance() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        // Create instance 01
        InstanceDto instance01 = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        assertNotNull(instance01);

        // Create instance 02
        InstanceDto instance02 = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        assertNotNull(instance02);

        // Check initial order
        List<InstanceBaseDto> instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationDto.getId());
        assertEquals(instance02.getId(), instances.get(0).getId());
        assertEquals(instance01.getId(), instances.get(1).getId());

        assertEquals(Integer.valueOf(1), instances.get(0).getOrder());
        assertEquals(Integer.valueOf(0), instances.get(1).getOrder());

        // Reorder instances
        List<Long> newOrder = new ArrayList<Long>();
        newOrder.add(instance02.getId());
        newOrder.add(instance01.getId());
        statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContext(), operationDto.getId(), newOrder);

        // Check order
        instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationDto.getId());
        assertEquals(instance01.getId(), instances.get(0).getId());
        assertEquals(instance02.getId(), instances.get(1).getId());

        assertEquals(Integer.valueOf(0), instances.get(1).getOrder());
        assertEquals(Integer.valueOf(1), instances.get(0).getOrder());

        // Create instance 03
        InstanceDto instance03 = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        assertNotNull(instance03);

        // Check order
        instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationDto.getId());
        assertEquals(instance02.getId(), instances.get(2).getId());
        assertEquals(instance01.getId(), instances.get(1).getId());
        assertEquals(instance03.getId(), instances.get(0).getId());

        assertEquals(Integer.valueOf(0), instances.get(2).getOrder());
        assertEquals(Integer.valueOf(1), instances.get(1).getOrder());
        assertEquals(Integer.valueOf(2), instances.get(0).getOrder());

        // Delete instance
        statisticalOperationsServiceFacade.deleteInstance(getServiceContext(), instance02.getId());

        // Check for deleted instance
        try {
            statisticalOperationsServiceFacade.findInstanceById(getServiceContext(), instance01.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INSTANCE_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check order
        instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationDto.getId());
        assertEquals(instance01.getId(), instances.get(1).getId());
        assertEquals(instance03.getId(), instances.get(0).getId());

        assertEquals(Integer.valueOf(0), instances.get(1).getOrder());
        assertEquals(Integer.valueOf(1), instances.get(0).getOrder());

    }

    @Test
    public void testCheckCurrentInstances() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());
        statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContext(), operationDto.getId());

        // Create instance 01
        InstanceDto instance01 = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        assertNotNull(instance01);
        statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContext(), instance01.getId());
        statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContext(), instance01.getId());

        // Create instance 02
        InstanceDto instance02 = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        assertNotNull(instance02);

        // Check initial order
        List<InstanceBaseDto> instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationDto.getId());
        assertEquals(instance02.getId(), instances.get(0).getId());
        assertEquals(instance01.getId(), instances.get(1).getId());

        assertEquals(Integer.valueOf(1), instances.get(0).getOrder());
        assertEquals(Integer.valueOf(0), instances.get(1).getOrder());

        // Check current instances
        operationDto = statisticalOperationsServiceFacade.findOperationById(getServiceContext(), operationDto.getId());
        assertEquals(instance01.getId(), operationDto.getCurrentInstance().getId());
        assertEquals(null, operationDto.getCurrentInternalInstance());

        // Reorder instances
        List<Long> newOrder = new ArrayList<Long>();
        newOrder.add(instance02.getId());
        newOrder.add(instance01.getId());
        statisticalOperationsServiceFacade.updateInstancesOrder(getServiceContext(), operationDto.getId(), newOrder);

        // Check order
        instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationDto.getId());
        assertEquals(instance01.getId(), instances.get(0).getId());
        assertEquals(instance02.getId(), instances.get(1).getId());

        assertEquals(Integer.valueOf(0), instances.get(1).getOrder());
        assertEquals(Integer.valueOf(1), instances.get(0).getOrder());

        // Check current instances
        operationDto = statisticalOperationsServiceFacade.findOperationById(getServiceContext(), operationDto.getId());
        assertEquals(instance01.getId(), operationDto.getCurrentInstance().getId());
        assertEquals(null, operationDto.getCurrentInternalInstance());

        // Create instance 03
        InstanceDto instance03 = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        assertNotNull(instance03);
        statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContext(), instance03.getId());

        // Check order
        instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationDto.getId());
        assertEquals(instance02.getId(), instances.get(2).getId());
        assertEquals(instance01.getId(), instances.get(1).getId());
        assertEquals(instance03.getId(), instances.get(0).getId());

        assertEquals(Integer.valueOf(0), instances.get(2).getOrder());
        assertEquals(Integer.valueOf(1), instances.get(1).getOrder());
        assertEquals(Integer.valueOf(2), instances.get(0).getOrder());

        // Check current instances
        operationDto = statisticalOperationsServiceFacade.findOperationById(getServiceContext(), operationDto.getId());
        assertEquals(instance01.getId(), operationDto.getCurrentInstance().getId());
        assertEquals(instance03.getId(), operationDto.getCurrentInternalInstance().getId());

        // Delete instance
        statisticalOperationsServiceFacade.deleteInstance(getServiceContext(), instance02.getId());

        // Publish externally instance03
        statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContext(), instance03.getId());

        // Check for deleted instance
        try {
            statisticalOperationsServiceFacade.findInstanceById(getServiceContext(), instance01.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INSTANCE_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }

        // Check order
        instances = statisticalOperationsServiceFacade.findInstancesForOperation(getServiceContext(), operationDto.getId());
        assertEquals(instance01.getId(), instances.get(1).getId());
        assertEquals(instance03.getId(), instances.get(0).getId());

        assertEquals(Integer.valueOf(0), instances.get(1).getOrder());
        assertEquals(Integer.valueOf(1), instances.get(0).getOrder());

        // Check current instances
        operationDto = statisticalOperationsServiceFacade.findOperationById(getServiceContext(), operationDto.getId());
        assertEquals(instance03.getId(), operationDto.getCurrentInstance().getId());
        assertEquals(null, operationDto.getCurrentInternalInstance());

    }

    // @Test
    // public void testDeleteInstanceById() throws MetamacException {
    // initializeData();
    //
    // List<InstanceDto> instances = statisticalOperationsServiceFacade.findAllInstances(getServiceContext());
    //
    // int numberInstances = instances.size();
    // InstanceDto instanceDto = instances.get(0);
    //
    // statisticalOperationsServiceFacade.deleteInstance(getServiceContext(), instanceDto.getId());
    // assertTrue(statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size() == (numberInstances - 1));
    //
    // try {
    // statisticalOperationsServiceFacade.findInstanceById(getServiceContext(), instanceDto.getId());
    // } catch (MetamacException e) {
    // assertEquals(ServiceExceptionType.SERVICE_INSTANCE_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
    // }
    //
    // }

    @Test
    public void testDeleteInstanceInternallyPublished() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        int instancesBeforeCreateInstance = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        assertNotNull(instanceDto);

        int instancesBeforeDelete = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
        assertEquals(instancesBeforeDelete, instancesBeforeCreateInstance + 1);

        // Set PUBLISH_INTERNALLY procStatus
        instanceDto = statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContext(), instanceDto.getId());

        // Delete instance with an incorrect procStatus
        try {
            statisticalOperationsServiceFacade.deleteInstance(getServiceContext(), instanceDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }

        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
        assertEquals(instancesBeforeDelete, instancesAfter);
    }

    @Test
    public void testDeleteInstanceExternallyPublished() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());
        operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContext(), operationDto.getId());

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());
        assertNotNull(instanceDto);

        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();

        // Set PUBLISH_INTERNALLY procStatus
        instanceDto = statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContext(), instanceDto.getId());
        instanceDto = statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContext(), instanceDto.getId());

        // Delete instance with an incorrect procStatus
        try {
            statisticalOperationsServiceFacade.deleteInstance(getServiceContext(), instanceDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(1, e.getExceptionItems().size());
        }

        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
        assertEquals(instancesBefore, instancesAfter);
    }

    @Test
    public void testFindAllInstances() throws MetamacException {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationId);
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationId, createInstanceDto());
        assertNotNull(instanceDto);

        List<InstanceBaseDto> instances = statisticalOperationsServiceFacade.findAllInstances(getServiceContext());
        assertTrue(!instances.isEmpty());
    }

    @Test
    public void testFindInstanceByCondition() throws MetamacException {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationId);

        statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationId, createInstanceDto());
        statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationId, createInstanceDto());
        statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationId, createInstanceDto());

        MetamacCriteria criteria = new MetamacCriteria();
        MetamacCriteriaResult<InstanceBaseDto> result = statisticalOperationsServiceFacade.findInstanceByCondition(getServiceContext(), criteria);
        assertTrue(result.getResults().size() >= 2);
    }

    @Test
    public void testFindInstanceByConditionPaginated() throws MetamacException {
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationId);

        statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationId, createInstanceDto());
        statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationId, createInstanceDto());
        statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationId, createInstanceDto());

        MetamacCriteria criteria = new MetamacCriteria();

        MetamacCriteriaPaginator paginator = new MetamacCriteriaPaginator();
        paginator.setCountTotalResults(Boolean.TRUE);
        paginator.setMaximumResultSize(Integer.valueOf(2));
        criteria.setPaginator(paginator);

        {
            // Page 1
            criteria.getPaginator().setFirstResult(Integer.valueOf(0));

            MetamacCriteriaResult<InstanceBaseDto> result = statisticalOperationsServiceFacade.findInstanceByCondition(getServiceContext(), criteria);
            assertEquals(2, result.getResults().size());
            assertTrue(result.getPaginatorResult().getTotalResults() >= 3);
            assertEquals(Integer.valueOf(0), result.getPaginatorResult().getFirstResult());
        }

        {
            // Page 2
            criteria.getPaginator().setFirstResult(Integer.valueOf(2));
            MetamacCriteriaResult<InstanceBaseDto> result = statisticalOperationsServiceFacade.findInstanceByCondition(getServiceContext(), criteria);
            assertTrue(result.getPaginatorResult().getTotalResults() >= 1);
            assertEquals(Integer.valueOf(2), result.getPaginatorResult().getFirstResult());

        }
    }

    @Test
    public void testFindInstanceById() throws MetamacException {
        // Create instance
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());

        InstanceDto instanceRetrieved = statisticalOperationsServiceFacade.findInstanceById(getServiceContext(), instanceDto.getId());

        assertNotNull(instanceRetrieved);
        assertTrue(instanceDto.getId().equals(instanceRetrieved.getId()));
    }

    @Test
    public void testPublishInternallyInstance() throws MetamacException {
        // Service doesn't check if the associated operation is PUBLISHED_INTERNALLY because it's a requirement for create it.

        // Create a PUBLISH_INTERNALLY Operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());
        assertTrue(ProcStatusEnum.PUBLISH_INTERNALLY.equals(operationDto.getProcStatus()));

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());

        int instancesBeforePublish = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
        int operationsBeforePublish = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();

        // Publish instance
        instanceDto = statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContext(), instanceDto.getId());

        // Validations
        assertNotNull(operationDto.getInternalInventoryDate());
        assertTrue(ProcStatusEnum.PUBLISH_INTERNALLY.equals(instanceDto.getProcStatus()));

        // Check number of operations and instances
        int instancesAfterPublish = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
        int operationsAfterPublish = statisticalOperationsServiceFacade.findAllOperations(getServiceContext()).size();
        assertEquals(instancesBeforePublish, instancesAfterPublish);
        assertEquals(operationsBeforePublish, operationsAfterPublish);
    }

    @Test
    public void testPublishExternallyInstance() throws MetamacException {
        // Create a PUBLISH_EXTERNALLY Operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());
        operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContext(), operationDto.getId());
        assertTrue(ProcStatusEnum.PUBLISH_EXTERNALLY.equals(operationDto.getProcStatus()));

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());

        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();

        // Publish instance
        instanceDto = statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContext(), instanceDto.getId());
        instanceDto = statisticalOperationsServiceFacade.publishExternallyInstance(getServiceContext(), instanceDto.getId());

        // Validations
        assertNotNull(operationDto.getInternalInventoryDate());
        assertNotNull(operationDto.getInventoryDate());
        assertTrue(ProcStatusEnum.PUBLISH_EXTERNALLY.equals(instanceDto.getProcStatus()));

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
        assertEquals(instancesBefore, instancesAfter);

        // Check that instance can't be internally published
        try {
            statisticalOperationsServiceFacade.publishInternallyInstance(getServiceContext(), instanceDto.getId());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INVALID_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testFindOperationForInstance() throws MetamacException {
        // Create operation
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        // Create instance
        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationDto.getId(), createInstanceDto());

        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();

        OperationBaseDto operationRetrieved = statisticalOperationsServiceFacade.findOperationForInstance(getServiceContext(), instanceDto.getId());

        assertEquals(operationRetrieved.getId(), operationDto.getId());

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
        assertEquals(instancesBefore, instancesAfter);
    }

    @Test
    public void testFindInstanceBaseById() throws MetamacException {
        // Create instance
        Long operationId = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing()).getId();
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationId);

        InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(getServiceContext(), operationId, createInstanceDto());

        int instancesBefore = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();

        InstanceBaseDto instanceRetrieved = statisticalOperationsServiceFacade.findInstanceBaseById(getServiceContext(), instanceDto.getId());

        assertNotNull(instanceRetrieved);
        assertTrue(instanceDto.getId().equals(instanceRetrieved.getId()));

        // Check number of instances
        int instancesAfter = statisticalOperationsServiceFacade.findAllInstances(getServiceContext()).size();
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
        operationDto.setSurveyType(statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContext(), Long.valueOf(1)));

        // OFFICIALITY_TYPE
        operationDto.setOfficialityType(statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContext(), Long.valueOf(1)));

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
        operationDto.setSurveyType(statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContext(), Long.valueOf(1)));

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
        operationDto.setOfficialityType(statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContext(), Long.valueOf(1)));

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
        instanceDto.setInstanceType(statisticalOperationsServiceFacade.findInstanceTypeById(getServiceContext(), Long.valueOf(1)));

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
