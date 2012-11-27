package org.siemac.metamac.statistical.operations.core.serviceapi;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
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
public class StatisticalOperationsListsServiceTest extends StatisticalOperationsBaseTest implements StatisticalOperationsListsServiceTestBase {

    @Autowired
    protected StatisticalOperationsListsService statisticalOperationsListsService;

    @Test
    public void testFindSurveyTypeById() throws Exception {
        SurveyType surveyType = statisticalOperationsListsService.findSurveyTypeById(getServiceContextAdministrador(), Long.valueOf(1));
        assertNotNull(surveyType);
    }

    @Test
    public void testFindAllSurveyTypes() throws Exception {
        List<SurveyType> surveyTypesList = statisticalOperationsListsService.findAllSurveyTypes(getServiceContextAdministrador());
        assertTrue(!surveyTypesList.isEmpty());
    }

    @Test
    public void testFindInstanceTypeById() throws Exception {
        InstanceType instanceType = statisticalOperationsListsService.findInstanceTypeById(getServiceContextAdministrador(), Long.valueOf(1));
        assertNotNull(instanceType);
    }

    @Test
    public void testFindAllInstanceTypes() throws Exception {
        List<InstanceType> instanceTypesList = statisticalOperationsListsService.findAllInstanceTypes(getServiceContextAdministrador());
        assertTrue(!instanceTypesList.isEmpty());
    }

    @Test
    public void testFindSurveySourceById() throws Exception {
        SurveySource surveySource = statisticalOperationsListsService.findSurveySourceById(getServiceContextAdministrador(), Long.valueOf(1));
        assertNotNull(surveySource);
    }

    @Test
    public void testFindAllSurveySources() throws Exception {
        List<SurveySource> sourcesDataList = statisticalOperationsListsService.findAllSurveySources(getServiceContextAdministrador());
        assertTrue(!sourcesDataList.isEmpty());

    }

    @Test
    public void testFindOfficialityTypeById() throws Exception {
        OfficialityType officialityType = statisticalOperationsListsService.findOfficialityTypeById(getServiceContextAdministrador(), Long.valueOf(1));
        assertNotNull(officialityType);
    }

    @Test
    public void testFindAllOfficialityTypes() throws Exception {
        List<OfficialityType> officialityTypesList = statisticalOperationsListsService.findAllOfficialityTypes(getServiceContextAdministrador());
        assertTrue(!officialityTypesList.isEmpty());
    }

    @Test
    public void testFindCollMethodById() throws Exception {
        CollMethod collMethod = statisticalOperationsListsService.findCollMethodById(getServiceContextAdministrador(), Long.valueOf(1));
        assertNotNull(collMethod);

    }

    @Test
    public void testFindAllCollMethods() throws Exception {
        List<CollMethod> collMethodsList = statisticalOperationsListsService.findAllCollMethods(getServiceContextAdministrador());
        assertTrue(!collMethodsList.isEmpty());

    }

    @Test
    public void testFindCostById() throws Exception {
        Cost cost = statisticalOperationsListsService.findCostById(getServiceContextAdministrador(), Long.valueOf(1));
        assertNotNull(cost);

    }

    @Test
    public void testFindAllCosts() throws Exception {
        List<Cost> costsList = statisticalOperationsListsService.findAllCosts(getServiceContextAdministrador());
        assertTrue(!costsList.isEmpty());

    }

}
