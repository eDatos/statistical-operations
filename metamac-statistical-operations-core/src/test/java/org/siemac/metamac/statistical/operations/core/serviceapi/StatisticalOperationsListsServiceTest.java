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
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Spring based transactional test with DbUnit support.
 */

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:oracle/applicationContext-test.xml"})
public class StatisticalOperationsListsServiceTest extends StatisticalOperationsBaseTest implements StatisticalOperationsListsServiceTestBase {

    @Autowired
    protected StatisticalOperationsListsService statisticalOperationsListsService;

    @Test
    public void testFindSurveyTypeById() throws Exception {
        SurveyType surveyType = statisticalOperationsListsService.findSurveyTypeById(getServiceContext(), Long.valueOf(1));
        assertNotNull(surveyType);
    }

    @Test
    public void testFindAllSurveyTypes() throws Exception {
        List<SurveyType> surveyTypesList = statisticalOperationsListsService.findAllSurveyTypes(getServiceContext());
        assertTrue(!surveyTypesList.isEmpty());
    }

    @Test
    public void testFindInstanceTypeById() throws Exception {
        InstanceType instanceType = statisticalOperationsListsService.findInstanceTypeById(getServiceContext(), Long.valueOf(1));
        assertNotNull(instanceType);
    }

    @Test
    public void testFindAllInstanceTypes() throws Exception {
        List<InstanceType> instanceTypesList = statisticalOperationsListsService.findAllInstanceTypes(getServiceContext());
        assertTrue(!instanceTypesList.isEmpty());
    }

    @Test
    public void testFindSurveySourceById() throws Exception {
        SurveySource surveySource = statisticalOperationsListsService.findSurveySourceById(getServiceContext(), Long.valueOf(1));
        assertNotNull(surveySource);
    }

    @Test
    public void testFindAllSurveySources() throws Exception {
        List<SurveySource> sourcesDataList = statisticalOperationsListsService.findAllSurveySources(getServiceContext());
        assertTrue(!sourcesDataList.isEmpty());

    }

    @Test
    public void testFindOfficialityTypeById() throws Exception {
        OfficialityType officialityType = statisticalOperationsListsService.findOfficialityTypeById(getServiceContext(), Long.valueOf(1));
        assertNotNull(officialityType);
    }

    @Test
    public void testFindAllOfficialityTypes() throws Exception {
        List<OfficialityType> officialityTypesList = statisticalOperationsListsService.findAllOfficialityTypes(getServiceContext());
        assertTrue(!officialityTypesList.isEmpty());
    }

    @Test
    public void testFindCollMethodById() throws Exception {
        CollMethod collMethod = statisticalOperationsListsService.findCollMethodById(getServiceContext(), Long.valueOf(1));
        assertNotNull(collMethod);

    }

    @Test
    public void testFindAllCollMethods() throws Exception {
        List<CollMethod> collMethodsList = statisticalOperationsListsService.findAllCollMethods(getServiceContext());
        assertTrue(!collMethodsList.isEmpty());

    }

    @Test
    public void testFindCostById() throws Exception {
        Cost cost = statisticalOperationsListsService.findCostById(getServiceContext(), Long.valueOf(1));
        assertNotNull(cost);

    }

    @Test
    public void testFindAllCosts() throws Exception {
        List<Cost> costsList = statisticalOperationsListsService.findAllCosts(getServiceContext());
        assertTrue(!costsList.isEmpty());

    }

}
