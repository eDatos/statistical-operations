package org.siemac.metamac.statistical.operations.core.serviceapi;

import static org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder.criteriaFor;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.RandomStringUtils;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.fornax.cartridges.sculptor.framework.domain.PagingParameter;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.siemac.metamac.core.common.bt.domain.ExternalItemBt;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.vo.domain.ExternalItem;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionParameters;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.transaction.TransactionConfiguration;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.DefaultTransactionDefinition;

/**
 * Spring based transactional test with DbUnit support.
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:spring/statistical-operations/applicationContext-test.xml"})
@TransactionConfiguration(transactionManager = "txManager", defaultRollback = false)
@Transactional
public class StatisticalOperationsBaseServiceTest extends StatisticalOperationsBaseTest implements StatisticalOperationsBaseServiceTestBase {

    @Autowired
    protected StatisticalOperationsBaseService  statisticalOperationsBaseService;

    @Autowired
    protected StatisticalOperationsListsService statisticalOperationsListsService;

    @Autowired
    private PlatformTransactionManager          transactionManager = null;

    /**************************************************************************
     * Family Tests
     **************************************************************************/

    @Test
    public void testFindFamilyById() throws MetamacException {
        Family family = statisticalOperationsBaseService.createFamily(getServiceContextAdministrador(), createFamily());
        assertNotNull(family);

        Family familyRetrieved = statisticalOperationsBaseService.findFamilyById(getServiceContextAdministrador(), family.getId());
        assertNotNull(familyRetrieved);

        assertTrue(family.equals(familyRetrieved));
    }

    @Test
    public void testFindFamilyByCode() throws Exception {
        String family_code = "family_01";

        Family family = createFamily();
        family.setCode(family_code);

        statisticalOperationsBaseService.createFamily(getServiceContextAdministrador(), family);

        Family familyRetrieved = statisticalOperationsBaseService.findFamilyByCode(getServiceContextAdministrador(), family_code);
        assertNotNull(familyRetrieved);
        assertEquals(family_code, family.getCode());
    }

    @Test
    public void testFindFamilyByCodeNotExists() throws Exception {
        String family_code = "not_exists";

        try {
            statisticalOperationsBaseService.findFamilyByCode(getServiceContextAdministrador(), family_code);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.FAMILY_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testSaveFamilyWithOperations() throws MetamacException {
        Family family = statisticalOperationsBaseService.createFamily(getServiceContextAdministrador(), createFamilyWithOperations());
        assertNotNull(family);
    }

    @Test
    public void testDeleteFamily() throws MetamacException {
        Family family = statisticalOperationsBaseService.createFamily(getServiceContextAdministrador(), createFamily());

        List<Family> families = statisticalOperationsBaseService.findAllFamilies(getServiceContextAdministrador());

        statisticalOperationsBaseService.deleteFamily(getServiceContextAdministrador(), family.getId());

        assertTrue(statisticalOperationsBaseService.findAllFamilies(getServiceContextAdministrador()).size() < families.size());
    }

    @Test
    public void testFindAllFamilies() throws MetamacException {
        Family family = statisticalOperationsBaseService.createFamily(getServiceContextAdministrador(), createFamily());
        assertNotNull(family);

        List<Family> families = statisticalOperationsBaseService.findAllFamilies(getServiceContextAdministrador());
        assertTrue(!families.isEmpty());
    }

    @Test
    public void testFindFamilyByCondition() throws MetamacException {
        statisticalOperationsBaseService.createFamily(getServiceContextAdministrador(), createFamily());

        List<ConditionalCriteria> conditions = criteriaFor(Family.class).withProperty(org.siemac.metamac.statistical.operations.core.domain.FamilyProperties.code()).like("FAMILY-%").distinctRoot()
                .build();

        List<Family> familiesList = statisticalOperationsBaseService.findFamilyByCondition(getServiceContextAdministrador(), conditions);

        assertTrue(familiesList.size() != 0);
    }

    @Test
    public void testFindFamilyByConditionPaginated() throws MetamacException {
        statisticalOperationsBaseService.createFamily(getServiceContextAdministrador(), createFamily());
        statisticalOperationsBaseService.createFamily(getServiceContextAdministrador(), createFamily());
        statisticalOperationsBaseService.createFamily(getServiceContextAdministrador(), createFamily());

        PagingParameter pagingParameterPage1 = PagingParameter.pageAccess(2, 1, true);
        PagingParameter pagingParameterPage2 = PagingParameter.pageAccess(2, 2, true);

        List<ConditionalCriteria> conditions = criteriaFor(Family.class).withProperty(org.siemac.metamac.statistical.operations.core.domain.FamilyProperties.code()).like("FAMILY-%").distinctRoot()
                .build();

        PagedResult<Family> familiesList1 = statisticalOperationsBaseService.findFamilyByCondition(getServiceContextAdministrador(), conditions, pagingParameterPage1);
        PagedResult<Family> familiesList2 = statisticalOperationsBaseService.findFamilyByCondition(getServiceContextAdministrador(), conditions, pagingParameterPage2);

        assertTrue(familiesList1.getValues().size() != 0);
        assertEquals(2, familiesList1.getPageSize());
        assertEquals(2, familiesList1.getRowCount());
        assertTrue(familiesList1.getTotalPages() >= 2);

        assertTrue(familiesList2.getValues().size() != 0);
        assertEquals(2, familiesList2.getPageSize());
    }

    @Test
    public void testCreateFamily() throws Exception {
        Family family = statisticalOperationsBaseService.createFamily(getServiceContextAdministrador(), createFamily());
        assertNotNull(family);
    }

    @Test
    public void testUpdateFamily() throws Exception {
        Family family = statisticalOperationsBaseService.createFamily(getServiceContextAdministrador(), createFamily());
        assertNotNull(family);

        // TITLE
        InternationalString title = new InternationalString();
        LocalisedString title_es = new LocalisedString();
        title_es.setLabel("Título MOD en español de familia");
        title_es.setLocale("es");
        LocalisedString title_en = new LocalisedString();
        title_en.setLabel("Título MOD en inglés de familia");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        family.setTitle(title);

        family = statisticalOperationsBaseService.updateFamily(getServiceContextAdministrador(), family);
        assertNotNull(family);
    }

    @Test
    public void testPublishInternallyFamily() throws Exception {
        // This test is in *ServiceFacade

    }

    @Test
    public void testPublishExternallyFamily() throws Exception {
        // This test is in *ServiceFacade

    }

    /**************************************************************************
     * Operation Tests
     **************************************************************************/

    @Test
    public void testFindOperationById() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperation());
        assertNotNull(operation);

        Operation operationRetrieved = statisticalOperationsBaseService.findOperationById(getServiceContextAdministrador(), operation.getId());
        assertNotNull(operationRetrieved);

        assertTrue(operation.equals(operationRetrieved));
    }

    @Test
    public void testFindOperationByCode() throws Exception {
        String operation_code = "operation_01";

        Operation operation = createOperation();
        operation.setCode(operation_code);

        statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), operation);

        Operation operationRetrieved = statisticalOperationsBaseService.findOperationByCode(getServiceContextAdministrador(), operation_code);
        assertNotNull(operationRetrieved);
        assertEquals(operation_code, operation.getCode());
    }

    @Test
    public void testFindOperationByCodeNotExists() throws Exception {
        String operation_code = "not_exists";

        try {
            statisticalOperationsBaseService.findOperationByCode(getServiceContextAdministrador(), operation_code);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.OPERATION_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testCreateOperation() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperation());
        assertNotNull(operation);
    }

    @Test
    public void testCreateOperationWithoutDescription() throws MetamacException {
        Operation operation = createOperation();
        operation.setDescription(null);

        operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), operation);

        assertNotNull(operation);
        assertEquals(null, operation.getDescription());
    }

    @Test
    public void testCreateOperationWithInvalidAccess() throws MetamacException {
        Operation operation = createOperation();

        operation.setReleaseCalendarAccess("invalidUrl");

        try {
            operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), operation);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.METADATA_INVALID_URL.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testSaveOperationWithFamilies() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationWithFamilies());
        assertNotNull(operation);
    }

    @Test
    public void testDeleteOperation() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperation());
        assertNotNull(operation);
        List<Operation> operations = statisticalOperationsBaseService.findAllOperations(getServiceContextAdministrador());

        statisticalOperationsBaseService.deleteOperation(getServiceContextAdministrador(), operation.getId());

        assertTrue(statisticalOperationsBaseService.findAllOperations(getServiceContextAdministrador()).size() < operations.size());
    }

    @Test
    public void testDeleteOperationWithType() throws MetamacException {
        int operationsNumberBefore = statisticalOperationsBaseService.findAllOperations(getServiceContextAdministrador()).size();
        int operationsTypeBefore = statisticalOperationsListsService.findAllSurveyTypes(getServiceContextAdministrador()).size();

        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationWithType());
        statisticalOperationsBaseService.deleteOperation(getServiceContextAdministrador(), operation.getId());
        assertTrue(statisticalOperationsBaseService.findAllOperations(getServiceContextAdministrador()).size() <= operationsNumberBefore);
        assertTrue(statisticalOperationsListsService.findAllSurveyTypes(getServiceContextAdministrador()).size() == operationsTypeBefore);
    }

    @Test
    public void testFindAllOperations() throws MetamacException {
        statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperation());

        List<Operation> operations = statisticalOperationsBaseService.findAllOperations(getServiceContextAdministrador());
        assertTrue(!operations.isEmpty());

    }

    @Test
    public void testFindOperationByCondition() throws MetamacException {
        statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperation());

        List<ConditionalCriteria> conditions = criteriaFor(Operation.class).withProperty(org.siemac.metamac.statistical.operations.core.domain.OperationProperties.code()).like("OPERATION-%")
                .distinctRoot().build();

        List<Operation> operationsList = statisticalOperationsBaseService.findOperationByCondition(getServiceContextAdministrador(), conditions);

        assertTrue(operationsList.size() != 0);
    }

    @Test
    public void testFindOperationByConditionPaginated() throws MetamacException {
        statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperation());
        statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperation());
        statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperation());

        PagingParameter pagingParameterPage1 = PagingParameter.pageAccess(2, 1, true);
        PagingParameter pagingParameterPage2 = PagingParameter.pageAccess(2, 2, true);

        List<ConditionalCriteria> conditions = criteriaFor(Operation.class).withProperty(org.siemac.metamac.statistical.operations.core.domain.OperationProperties.code()).like("OPERATION-%")
                .distinctRoot().build();

        PagedResult<Operation> operationsList1 = statisticalOperationsBaseService.findOperationByCondition(getServiceContextAdministrador(), conditions, pagingParameterPage1);
        PagedResult<Operation> operationsList2 = statisticalOperationsBaseService.findOperationByCondition(getServiceContextAdministrador(), conditions, pagingParameterPage2);

        assertTrue(operationsList1.getValues().size() != 0);
        assertEquals(2, operationsList1.getPageSize());
        assertEquals(2, operationsList1.getRowCount());
        assertTrue(operationsList1.getTotalPages() >= 2);

        assertTrue(operationsList2.getValues().size() != 0);
        assertEquals(2, operationsList2.getPageSize());
    }

    @Test
    public void testAddOperationFamilyAssociation() throws Exception {
        // This test is in *ServiceFacade
    }

    @Test
    public void testRemoveOperationFamilyAssociation() throws Exception {
        // This test is in *ServiceFacade
    }

    @Test
    public void testUpdateOperation() throws Exception {
        Operation operation = createOperationWithDescription();
        operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), operation);
        assertNotNull(operation.getDescription());

        operation.setDescription(null);
        operation = statisticalOperationsBaseService.updateOperation(getServiceContextAdministrador(), operation);
        assertNotNull(operation);
        assertEquals(null, operation.getDescription());
    }

    @Test
    public void testUpdateOperationWithIncorrectReleaseCalendarAccess() throws Exception {
        DefaultTransactionDefinition defaultTransactionDefinition = new DefaultTransactionDefinition();
        defaultTransactionDefinition.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
        TransactionStatus status = transactionManager.getTransaction(defaultTransactionDefinition);

        Operation operation = createOperation();
        operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), operation);
        assertNull(operation.getReleaseCalendarAccess());

        transactionManager.commit(status);

        operation.setReleaseCalendarAccess("incorrectUrl");
        try {
            statisticalOperationsBaseService.updateOperation(getServiceContextAdministrador(), operation);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.METADATA_INVALID_URL.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(ServiceExceptionParameters.OPERATION_RELEASE_CALENDAR_ACCESS, e.getExceptionItems().get(0).getMessageParameters()[0]);
        }
    }

    @Test
    public void testUpdateOperationWitReleaseCalendarAccessAndWithoutReleaseCalendar() throws Exception {
        Operation operation = createOperation();
        operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), operation);
        assertNull(operation.getReleaseCalendarAccess());

        operation.setReleaseCalendar(Boolean.FALSE);
        operation.setReleaseCalendarAccess("http://tutu.com");
        try {
            statisticalOperationsBaseService.updateOperation(getServiceContextAdministrador(), operation);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.METADATA_INVALID_URL.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(ServiceExceptionParameters.OPERATION_RELEASE_CALENDAR_ACCESS, e.getExceptionItems().get(0).getMessageParameters()[0]);
        }
    }

    @Test
    public void testUpdateOperationWithoutOperation() throws Exception {
        Operation operation = createOperation();
        operation.setDescription(null);
        operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), operation);
        assertEquals(null, operation.getDescription());

        operation = statisticalOperationsBaseService.updateOperation(getServiceContextAdministrador(), operation);
        assertNotNull(operation);
        assertEquals(null, operation.getDescription());
    }

    @Test
    public void testPublishInternallyOperation() throws Exception {
        // This test is in *ServiceFacade
    }

    @Test
    public void testPublishExternallyOperation() throws Exception {
        // This test is in *ServiceFacade
    }

    /**************************************************************************
     * Instance Tests
     **************************************************************************/

    @Test
    public void testFindInstanceById() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationForInternalPublishing());
        statisticalOperationsBaseService.publishInternallyOperation(getServiceContextAdministrador(), operation.getId());
        Instance instance = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());
        assertNotNull(instance);

        Instance instanceRetrieved = statisticalOperationsBaseService.findInstanceById(getServiceContextAdministrador(), instance.getId());
        assertNotNull(instanceRetrieved);

        assertTrue(instance.equals(instanceRetrieved));
    }

    @Test
    public void testFindInstanceByCode() throws Exception {
        String instance_code = "instance_01";
        Instance instance = createInstance();
        instance.setCode(instance_code);

        Long operationId = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationForInternalPublishing()).getId();
        statisticalOperationsBaseService.publishInternallyOperation(getServiceContextAdministrador(), operationId);
        statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operationId, instance);

        Instance instanceRetrieved = statisticalOperationsBaseService.findInstanceByCode(getServiceContextAdministrador(), instance_code);
        assertNotNull(instanceRetrieved);
        assertEquals(instance_code, instance.getCode());
    }

    @Test
    public void testFindInstanceByCodeNotExists() throws Exception {
        String instance_code = "not_exists";

        try {
            statisticalOperationsBaseService.findInstanceByCode(getServiceContextAdministrador(), instance_code);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INSTANCE_NOT_FOUND.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testCreateInstance() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationForInternalPublishing());
        statisticalOperationsBaseService.publishInternallyOperation(getServiceContextAdministrador(), operation.getId());
        Instance instance = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());
        assertNotNull(instance);
    }

    @Test
    public void testCreateInstanceOperationNotPublished() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperation());
        try {
            statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.INSTANCE_INCORRECT_OPERATION_PROC_STATUS.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testDeleteInstance() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationForInternalPublishing());
        statisticalOperationsBaseService.publishInternallyOperation(getServiceContextAdministrador(), operation.getId());

        Instance instance = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());

        List<Instance> instances = statisticalOperationsBaseService.findAllInstances(getServiceContextAdministrador());

        statisticalOperationsBaseService.deleteInstance(getServiceContextAdministrador(), instance.getId());

        assertTrue(statisticalOperationsBaseService.findAllInstances(getServiceContextAdministrador()).size() < instances.size());
    }

    @Test
    public void testDeleteInstanceCheckUpdateOrder() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationForInternalPublishing());
        statisticalOperationsBaseService.publishInternallyOperation(getServiceContextAdministrador(), operation.getId());

        Instance instance01 = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());
        Instance instance02 = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());

        List<Instance> instances = statisticalOperationsBaseService.findAllInstances(getServiceContextAdministrador());

        statisticalOperationsBaseService.deleteInstance(getServiceContextAdministrador(), instance01.getId());

        instance02 = statisticalOperationsBaseService.findInstanceById(getServiceContextAdministrador(), instance02.getId());
        assertEquals(Integer.valueOf(0), instance02.getOrder());

        assertTrue(statisticalOperationsBaseService.findAllInstances(getServiceContextAdministrador()).size() < instances.size());
    }

    @Test
    public void testFindAllInstances() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationForInternalPublishing());
        statisticalOperationsBaseService.publishInternallyOperation(getServiceContextAdministrador(), operation.getId());

        statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());

        List<Instance> instances = statisticalOperationsBaseService.findAllInstances(getServiceContextAdministrador());
        assertTrue(!instances.isEmpty());
    }

    @Test
    public void testFindInstanceByCondition() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationForInternalPublishing());
        statisticalOperationsBaseService.publishInternallyOperation(getServiceContextAdministrador(), operation.getId());

        statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());

        List<ConditionalCriteria> conditions = criteriaFor(Instance.class).withProperty(org.siemac.metamac.statistical.operations.core.domain.InstanceProperties.code()).like("INSTANCE-%").build();

        List<Instance> instancesList = statisticalOperationsBaseService.findInstanceByCondition(getServiceContextAdministrador(), conditions);

        assertTrue(instancesList.size() != 0);
    }

    @Test
    public void testFindInstanceByConditionPaginated() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationForInternalPublishing());
        statisticalOperationsBaseService.publishInternallyOperation(getServiceContextAdministrador(), operation.getId());

        statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());
        statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());
        statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());

        PagingParameter pagingParameterPage1 = PagingParameter.pageAccess(2, 1, true);
        PagingParameter pagingParameterPage2 = PagingParameter.pageAccess(2, 2, true);

        List<ConditionalCriteria> conditions = criteriaFor(Instance.class).withProperty(org.siemac.metamac.statistical.operations.core.domain.InstanceProperties.code()).like("INSTANCE-%")
                .distinctRoot().build();

        PagedResult<Instance> instances1 = statisticalOperationsBaseService.findInstanceByCondition(getServiceContextAdministrador(), conditions, pagingParameterPage1);
        PagedResult<Instance> instances2 = statisticalOperationsBaseService.findInstanceByCondition(getServiceContextAdministrador(), conditions, pagingParameterPage2);

        assertTrue(instances1.getValues().size() != 0);
        assertEquals(2, instances1.getPageSize());
        assertEquals(2, instances1.getRowCount());
        assertTrue(instances1.getTotalPages() >= 2);

        assertTrue(instances2.getValues().size() != 0);
        assertEquals(2, instances2.getPageSize());
    }

    @Test
    public void testUpdateInstance() throws Exception {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationForInternalPublishing());
        statisticalOperationsBaseService.publishInternallyOperation(getServiceContextAdministrador(), operation.getId());

        Instance instance = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());
        instance.setBasePeriod("2005Q1");
        statisticalOperationsBaseService.updateInstance(getServiceContextAdministrador(), instance);
    }

    @Test
    public void testUpdateInstanceWithIncorrectBasePeriod() throws Exception {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationForInternalPublishing());
        statisticalOperationsBaseService.publishInternallyOperation(getServiceContextAdministrador(), operation.getId());

        Instance instance = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());
        instance.setBasePeriod("2005Q1error");
        try {
            statisticalOperationsBaseService.updateInstance(getServiceContextAdministrador(), instance);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.METADATA_INCORRECT.getCode(), e.getExceptionItems().get(0).getCode());
        }
    }

    @Test
    public void testUpdateInstancesOrder() throws Exception {
        // Create operation
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationForInternalPublishing());
        operation = statisticalOperationsBaseService.publishInternallyOperation(getServiceContextAdministrador(), operation.getId());

        // Create instances
        Instance instance01 = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());
        Instance instance02 = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());
        Instance instance03 = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());

        // Change order
        List<Long> instancesIds = new ArrayList<Long>();
        instancesIds.add(instance03.getId());
        instancesIds.add(instance02.getId());
        instancesIds.add(instance01.getId());

        List<Instance> orderedInstances = statisticalOperationsBaseService.updateInstancesOrder(getServiceContextAdministrador(), operation.getId(), instancesIds);

        // Check correct order
        assertEquals(orderedInstances.get(2).getId(), instance01.getId());
        assertEquals(orderedInstances.get(1).getId(), instance02.getId());
        assertEquals(orderedInstances.get(0).getId(), instance03.getId());
    }

    @Test
    public void testUpdateInstancesOrderIncorrectParameter() throws Exception {
        // Create operation
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperationForInternalPublishing());
        operation = statisticalOperationsBaseService.publishInternallyOperation(getServiceContextAdministrador(), operation.getId());

        // Create instances
        Instance instance01 = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());
        Instance instance02 = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());
        Instance instance03 = statisticalOperationsBaseService.createInstance(getServiceContextAdministrador(), operation.getId(), createInstance());

        // Change order
        List<Long> instancesIds = new ArrayList<Long>();
        instancesIds.add(instance03.getId());
        instancesIds.add(instance02.getId());

        try {
            statisticalOperationsBaseService.updateInstancesOrder(getServiceContextAdministrador(), operation.getId(), instancesIds);
        } catch (MetamacException e) {
            assertEquals(ServiceExceptionType.PARAMETER_INCORRECT.getCode(), e.getExceptionItems().get(0).getCode());
            assertEquals(ServiceExceptionParameters.INSTANCES_ID_LIST_SIZE, e.getExceptionItems().get(0).getMessageParameters()[0]);
        }
    }

    @Test
    public void testPublishInternallyInstance() throws Exception {
        // This test is in *ServiceFacade

    }

    @Test
    public void testPublishExternallyInstance() throws Exception {
        // This test is in *ServiceFacade

    }

    /*********************************************************************
     * MOCKS
     *********************************************************************/

    private Family createFamily() {
        Family family = new Family();

        // IDENTIFIER
        family.setCode("FAMILY-" + RandomStringUtils.random(50, true, true));

        // TITLE
        InternationalString title = new InternationalString();
        LocalisedString title_es = new LocalisedString();
        title_es.setLabel("Título en español de familia");
        title_es.setLocale("es");
        LocalisedString title_en = new LocalisedString();
        title_en.setLabel("Título en inglés de familia");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        family.setTitle(title);

        // DESCRIPTION
        InternationalString description = new InternationalString();
        LocalisedString description_es = new LocalisedString();
        description_es.setLabel("Descripción en español de familia");
        description_es.setLocale("es");
        LocalisedString description_en = new LocalisedString();
        description_en.setLabel("Descripción en inglés de familia");
        description_en.setLocale("en");
        description.addText(description_es);
        description.addText(description_en);
        family.setDescription(description);

        // PROC_STATUS
        family.setProcStatus(ProcStatusEnum.DRAFT);

        return family;
    }

    private Family createFamilyWithOperations() throws MetamacException {
        Family family = createFamily();

        // OPERATIONS
        for (int i = 0; i < 4; i++) {
            Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperation());
            family.getOperations().add(operation);
        }

        return family;
    }

    private Operation createOperation() {
        Operation operation = new Operation();

        // IDENTIFIER
        operation.setCode("OPERATION-" + RandomStringUtils.random(50, true, true));

        // TITLE
        InternationalString title = new InternationalString();
        LocalisedString title_es = new LocalisedString();
        title_es.setLabel("Título en español de operación");
        title_es.setLocale("es");
        LocalisedString title_en = new LocalisedString();
        title_en.setLabel("Título en inglés de operación");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        operation.setTitle(title);

        // ACRONYM
        InternationalString acronym = new InternationalString();
        LocalisedString acronym_es = new LocalisedString();
        acronym_es.setLabel("Acronynm en español de operación");
        acronym_es.setLocale("es");
        LocalisedString acronym_en = new LocalisedString();
        acronym_en.setLabel("Acronynm en inglés de operación");
        acronym_en.setLocale("en");
        acronym.addText(acronym_es);
        acronym.addText(acronym_en);
        operation.setAcronym(acronym);

        // PROC_STATUS
        operation.setProcStatus(ProcStatusEnum.DRAFT);

        // RELEASE_CALENDAR
        operation.setReleaseCalendar(false);

        // CURRENTLY_ACTIVE
        operation.setCurrentlyActive(false);

        // SUBJECT_AREA
        ExternalItemBt subjectArea = new ExternalItemBt("uri:internal:todo", "PRUEBA", TypeExternalArtefactsEnum.CATEGORY);
        operation.setSubjectArea(subjectArea);

        // INDICATOR_SYSTEM
        operation.setIndicatorSystem(false);

        return operation;
    }

    private Operation createOperationWithDescription() throws MetamacException {
        Operation operation = createOperation();

        // DESCRIPTION
        InternationalString description = new InternationalString();
        LocalisedString description_es = new LocalisedString();
        description_es.setLabel("Acronynm en español de operación");
        description_es.setLocale("es");
        LocalisedString description_en = new LocalisedString();
        description_en.setLabel("Acronynm en inglés de operación");
        description_en.setLocale("en");
        description.addText(description_es);
        description.addText(description_en);
        operation.setDescription(description);

        return operation;
    }

    private Operation createOperationWithType() throws MetamacException {
        Operation operation = createOperation();

        // TYPE
        operation.setSurveyType(statisticalOperationsListsService.findSurveyTypeById(getServiceContextAdministrador(), Long.valueOf(1)));

        return operation;
    }

    private Operation createOperationForInternalPublishing() throws MetamacException {
        Operation operation = createOperation();

        // OBJECTIVE
        InternationalString objective = new InternationalString();
        LocalisedString objective_es = new LocalisedString();
        objective_es.setLabel("OPERACION - OBJECTIVE - ES");
        objective_es.setLocale("es");
        LocalisedString objective_en = new LocalisedString();
        objective_en.setLabel("OPERACION - OBJECTIVE - EN");
        objective_en.setLocale("en");
        objective.addText(objective_es);
        objective.addText(objective_en);
        operation.setObjective(objective);

        // SURVEY_TYPE
        operation.setSurveyType(statisticalOperationsListsService.findSurveyTypeById(getServiceContextAdministrador(), Long.valueOf(1)));

        // PRODUCER
        ExternalItem producer01 = new ExternalItem(new ExternalItemBt("uri:internal:todo", "ISTAC", TypeExternalArtefactsEnum.AGENCY));
        operation.addProducer(producer01);

        ExternalItem producer02 = new ExternalItem(new ExternalItemBt("uri:internal:todo", "INE", TypeExternalArtefactsEnum.AGENCY));
        operation.addProducer(producer02);

        // REGIONAL_RESPONSIBLE
        ExternalItem regionalResponsible01 = new ExternalItem(new ExternalItemBt("uri:internal:todo", "ISTAC", TypeExternalArtefactsEnum.AGENCY));
        operation.addRegionalResponsible(regionalResponsible01);

        // PUBLISHER
        ExternalItem publisher01 = new ExternalItem(new ExternalItemBt("uri:internal:todo", "ISTAC", TypeExternalArtefactsEnum.AGENCY));
        operation.addPublisher(publisher01);

        // COMMON_METADATA
        ExternalItemBt commonMetadata = new ExternalItemBt("uri:internal:todo", "ISTAC", TypeExternalArtefactsEnum.AGENCY);
        operation.setCommonMetadata(commonMetadata);

        // OFFICIALITY_TYPE
        operation.setOfficialityType(statisticalOperationsListsService.findOfficialityTypeById(getServiceContextAdministrador(), Long.valueOf(1)));

        return operation;
    }

    private Operation createOperationWithFamilies() throws MetamacException {
        Operation operation = createOperation();

        // FAMILIES
        for (int i = 0; i < 4; i++) {
            Family family = statisticalOperationsBaseService.createFamily(getServiceContextAdministrador(), createFamily());
            operation.getFamilies().add(family);
        }

        return operation;
    }

    private Instance createInstance() throws MetamacException {
        Instance instance = new Instance();

        // IDENTIFIER
        instance.setCode("INSTANCE-" + RandomStringUtils.random(50, true, true));

        // TITLE
        InternationalString title = new InternationalString();
        LocalisedString title_es = new LocalisedString();
        title_es.setLabel("Título en español de instancia");
        title_es.setLocale("es");
        LocalisedString title_en = new LocalisedString();
        title_en.setLabel("Título en inglés de instancia");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        instance.setTitle(title);

        // OPERATION
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContextAdministrador(), createOperation());
        instance.setOperation(operation);

        // PROC_STATUS
        instance.setProcStatus(ProcStatusEnum.DRAFT);

        // ORDER
        instance.setOrder(0);

        return instance;
    }

}
