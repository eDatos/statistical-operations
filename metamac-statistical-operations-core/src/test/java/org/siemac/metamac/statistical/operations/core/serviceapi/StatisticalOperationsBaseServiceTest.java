package org.siemac.metamac.statistical.operations.core.serviceapi;

import static org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder.criteriaFor;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.apache.commons.lang.RandomStringUtils;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.siemac.metamac.common.test.MetamacBaseTests;
import org.siemac.metamac.core.common.bt.domain.ExternalItemBt;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Spring based transactional test with DbUnit support.
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:oracle/applicationContext-test.xml"})
public class StatisticalOperationsBaseServiceTest extends MetamacBaseTests implements StatisticalOperationsBaseServiceTestBase {

    @Autowired
    protected StatisticalOperationsBaseService  statisticalOperationsBaseService;

    @Autowired
    protected StatisticalOperationsListsService statisticalOperationsListsService;

    private final ServiceContext                serviceContext = new ServiceContext("system", "123456", "junit");

    protected ServiceContext getServiceContext() {
        return serviceContext;
    }

    /**************************************************************************
     * Family Tests
     **************************************************************************/

    @Test
    public void testFindFamilyById() throws MetamacException {
        Family family = statisticalOperationsBaseService.createFamily(getServiceContext(), createFamily());
        assertNotNull(family);

        Family familyRetrieved = statisticalOperationsBaseService.findFamilyById(getServiceContext(), family.getId());
        assertNotNull(familyRetrieved);

        assertTrue(family.equals(familyRetrieved));
    }

    @Test
    public void testSaveFamilyWithOperations() throws MetamacException {
        Family family = statisticalOperationsBaseService.createFamily(getServiceContext(), createFamilyWithOperations());
        assertNotNull(family);
    }

    @Test
    public void testDeleteFamily() throws MetamacException {
        Family family = statisticalOperationsBaseService.createFamily(getServiceContext(), createFamily());

        List<Family> families = statisticalOperationsBaseService.findAllFamilies(getServiceContext());

        statisticalOperationsBaseService.deleteFamily(getServiceContext(), family.getId());

        assertTrue(statisticalOperationsBaseService.findAllFamilies(getServiceContext()).size() < families.size());
    }

    @Test
    public void testFindAllFamilies() throws MetamacException {
        Family family = statisticalOperationsBaseService.createFamily(getServiceContext(), createFamily());
        assertNotNull(family);

        List<Family> families = statisticalOperationsBaseService.findAllFamilies(getServiceContext());
        assertTrue(!families.isEmpty());
    }

    @Test
    public void testFindFamilyByCondition() throws MetamacException {
        statisticalOperationsBaseService.createFamily(getServiceContext(), createFamily());

        List<ConditionalCriteria> conditions = criteriaFor(Family.class).withProperty(org.siemac.metamac.statistical.operations.core.domain.FamilyProperties.code()).like("FAMILY-%").build();

        List<Family> familiesList = statisticalOperationsBaseService.findFamilyByCondition(getServiceContext(), conditions);

        assertTrue(familiesList.size() != 0);
    }

    @Test
    public void testCreateFamily() throws Exception {
        Family family = statisticalOperationsBaseService.createFamily(getServiceContext(), createFamily());
        assertNotNull(family);
    }

    @Test
    public void testUpdateFamily() throws Exception {
        Family family = statisticalOperationsBaseService.createFamily(getServiceContext(), createFamily());
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

        family = statisticalOperationsBaseService.updateFamily(getServiceContext(), family);
        assertNotNull(family);
    }

    @Override
    public void testPublishInternallyFamily() throws Exception {
        // TODO Auto-generated method stub

    }

    @Override
    public void testPublishExternallyFamily() throws Exception {
        // TODO Auto-generated method stub

    }

    /**************************************************************************
     * Operation Tests
     **************************************************************************/

    @Test
    public void testFindOperationById() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContext(), createOperation());
        assertNotNull(operation);

        Operation operationRetrieved = statisticalOperationsBaseService.findOperationById(getServiceContext(), operation.getId());
        assertNotNull(operationRetrieved);

        assertTrue(operation.equals(operationRetrieved));
    }

    @Test
    public void testSaveOperation() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContext(), createOperation());
        assertNotNull(operation);
    }

    @Test
    public void testSaveOperationWithFamilies() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContext(), createOperationWithFamilies());
        assertNotNull(operation);
    }

    @Test
    public void testDeleteOperation() throws MetamacException {
        testSaveOperation();

        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContext(), createOperation());
        assertNotNull(operation);
        List<Operation> operations = statisticalOperationsBaseService.findAllOperations(getServiceContext());

        statisticalOperationsBaseService.deleteOperation(getServiceContext(), operation.getId());

        assertTrue(statisticalOperationsBaseService.findAllOperations(getServiceContext()).size() < operations.size());
    }

    @Test
    public void testDeleteOperationWithType() throws MetamacException {
        int operationsNumberBefore = statisticalOperationsBaseService.findAllOperations(getServiceContext()).size();
        int operationsTypeBefore = statisticalOperationsListsService.findAllSurveyTypes(getServiceContext()).size();

        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContext(), createOperationWithType());
        statisticalOperationsBaseService.deleteOperation(getServiceContext(), operation.getId());
        assertTrue(statisticalOperationsBaseService.findAllOperations(getServiceContext()).size() <= operationsNumberBefore);
        assertTrue(statisticalOperationsListsService.findAllSurveyTypes(getServiceContext()).size() == operationsTypeBefore);
    }

    @Test
    public void testFindAllOperations() throws MetamacException {
        testSaveOperation();

        List<Operation> operations = statisticalOperationsBaseService.findAllOperations(getServiceContext());
        assertTrue(!operations.isEmpty());

    }

    @Test
    public void testFindOperationByCondition() throws MetamacException {
        statisticalOperationsBaseService.createOperation(getServiceContext(), createOperation());

        List<ConditionalCriteria> conditions = criteriaFor(Operation.class).withProperty(org.siemac.metamac.statistical.operations.core.domain.OperationProperties.code()).like("OPERATION-%").build();

        List<Operation> operationsList = statisticalOperationsBaseService.findOperationByCondition(getServiceContext(), conditions);

        assertTrue(operationsList.size() != 0);
    }

    @Override
    public void testAddOperationFamilyAssociation() throws Exception {
        // TODO Auto-generated method stub

    }

    @Override
    public void testRemoveOperationFamilyAssociation() throws Exception {
        // TODO Auto-generated method stub

    }

    @Override
    public void testCreateOperation() throws Exception {
        // TODO Auto-generated method stub

    }

    @Override
    public void testUpdateOperation() throws Exception {
        // TODO Auto-generated method stub

    }

    @Override
    public void testPublishInternallyOperation() throws Exception {
        // TODO Auto-generated method stub

    }

    @Override
    public void testPublishExternallyOperation() throws Exception {
        // TODO Auto-generated method stub

    }

    /**************************************************************************
     * Instance Tests
     **************************************************************************/

    @Test
    public void testFindInstanceById() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContext(), createOperation());
        Instance instance = statisticalOperationsBaseService.createInstance(getServiceContext(), operation.getId(), createInstance());
        assertNotNull(instance);

        Instance instanceRetrieved = statisticalOperationsBaseService.findInstanceById(getServiceContext(), instance.getId());
        assertNotNull(instanceRetrieved);

        assertTrue(instance.equals(instanceRetrieved));
    }

    @Test
    public void testSaveInstance() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContext(), createOperation());
        Instance instance = statisticalOperationsBaseService.createInstance(getServiceContext(), operation.getId(), createInstance());
        assertNotNull(instance);
    }

    @Test
    public void testDeleteInstance() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContext(), createOperation());
        Instance instance = statisticalOperationsBaseService.createInstance(getServiceContext(), operation.getId(), createInstance());

        List<Instance> instances = statisticalOperationsBaseService.findAllInstances(getServiceContext());

        statisticalOperationsBaseService.deleteInstance(getServiceContext(), instance.getId());

        assertTrue(statisticalOperationsBaseService.findAllInstances(getServiceContext()).size() < instances.size());
    }

    @Test
    public void testFindAllInstances() throws MetamacException {
        testSaveInstance();

        List<Instance> instances = statisticalOperationsBaseService.findAllInstances(getServiceContext());
        assertTrue(!instances.isEmpty());
    }

    @Test
    public void testFindInstanceByCondition() throws MetamacException {
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContext(), createOperation());
        statisticalOperationsBaseService.createInstance(getServiceContext(), operation.getId(), createInstance());

        List<ConditionalCriteria> conditions = criteriaFor(Instance.class).withProperty(org.siemac.metamac.statistical.operations.core.domain.InstanceProperties.code()).like("INSTANCE-%").build();

        List<Instance> instancesList = statisticalOperationsBaseService.findInstanceByCondition(getServiceContext(), conditions);

        assertTrue(instancesList.size() != 0);
    }

    @Override
    public void testCreateInstance() throws Exception {
        // TODO Auto-generated method stub

    }

    @Override
    public void testUpdateInstance() throws Exception {
        // TODO Auto-generated method stub

    }

    @Override
    public void testUpdateInstancesOrder() throws Exception {
        // TODO Auto-generated method stub

    }

    @Override
    public void testPublishInternallyInstance() throws Exception {
        // TODO Auto-generated method stub

    }

    @Override
    public void testPublishExternallyInstance() throws Exception {
        // TODO Auto-generated method stub

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
            Operation operation = statisticalOperationsBaseService.createOperation(getServiceContext(), createOperation());
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

        // DESCRIPTION
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

    private Operation createOperationWithType() throws MetamacException {
        Operation operation = createOperation();

        // TYPE
        operation.setSurveyType(statisticalOperationsListsService.findSurveyTypeById(getServiceContext(), Long.valueOf(1)));

        return operation;
    }

    private Operation createOperationWithFamilies() throws MetamacException {
        Operation operation = createOperation();

        // FAMILIES
        for (int i = 0; i < 4; i++) {
            Family family = statisticalOperationsBaseService.createFamily(getServiceContext(), createFamily());
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
        Operation operation = statisticalOperationsBaseService.createOperation(getServiceContext(), createOperation());
        instance.setOperation(operation);

        // PROC_STATUS
        instance.setProcStatus(ProcStatusEnum.DRAFT);

        // ORDER
        instance.setOrder(0);

        return instance;
    }

    /**************************************************************************
     * DBUNIT CONFIGURATION
     **************************************************************************/

    @Override
    protected String getDataSetFile() {
        return "dbunit/StatisticalOperationsBaseServiceTest.xml";
    }

    @Override
    protected List<String> getTablesToRemoveContent() {
        return null;
    }

    @Override
    protected List<String> getSequencesToRestart() {
        return null;
    }

    @Override
    public void tearDownDatabaseTester() throws Exception {
        // NOTHING;
    }
}
