package org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.siemac.metamac.rest.common.test.utils.MetamacRestAsserts;
import org.siemac.metamac.rest.common.v1_0.domain.RelatedResource;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.FamiliesNoPagedResult;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationsPagedResult;

public class StatisticalOperationsRestAsserts {

    public static void assertEqualsOperation(Operation expected, Operation actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        assertEquals(expected.getSelfLink(), actual.getSelfLink());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getFamilies(), actual.getFamilies());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getSubjectArea(), actual.getSubjectArea());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getSecondarySubjectAreas(), actual.getSecondarySubjectAreas());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getObjective(), actual.getObjective());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getInstances(), actual.getInstances());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getSurveyType(), actual.getSurveyType());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getOfficialityType(), actual.getOfficialityType());
        assertEquals(expected.isIndicatorSystem(), actual.isIndicatorSystem());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getProducers(), actual.getProducers());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getRegionalResponsibles(), actual.getRegionalResponsibles());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getRegionalContributors(), actual.getRegionalContributors());
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.isCurrentlyActive(), actual.isCurrentlyActive());
        assertEquals(expected.getStatus(), actual.getStatus());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getPublishers(), actual.getPublishers());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getRelPolUsAc(), actual.getRelPolUsAc());
        assertEquals(expected.isReleaseCalendar(), actual.isReleaseCalendar());
        assertEquals(expected.getReleaseCalendarAccess(), actual.getReleaseCalendarAccess());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getUpdateFrequencies(), actual.getUpdateFrequencies());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getCurrentInternalInstance(), actual.getCurrentInternalInstance());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getCurrentInstance(), actual.getCurrentInstance());
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getRevPolicy(), actual.getRevPolicy());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getRevPractice(), actual.getRevPractice());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getContacts(), actual.getContacts());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getLegalActs(), actual.getLegalActs());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDataSharing(), actual.getDataSharing());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getConfidentialityPolicy(), actual.getConfidentialityPolicy());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getConfidentialityDataTreatment(), actual.getConfidentialityDataTreatment());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getNotes(), actual.getNotes());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getParent(), actual.getParent());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getchildren(), actual.getchildren());
    }

    public static void assertEqualsFamily(Family expected, Family actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        assertEquals(expected.getSelfLink(), actual.getSelfLink());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getParent(), actual.getParent());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getchildren(), actual.getchildren());
    }

    public static void assertEqualsInstance(Instance expected, Instance actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        assertEquals(expected.getSelfLink(), actual.getSelfLink());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getSurvey(), actual.getSurvey());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getSuccessor(), actual.getSuccessor());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getPredecessor(), actual.getPredecessor());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDataDescription(), actual.getDataDescription());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getStatisticalPopulation(), actual.getStatisticalPopulation());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getStatisticalUnits(), actual.getStatisticalUnits());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getGeographicGranularity(), actual.getGeographicGranularity());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getGeographicComparability(), actual.getGeographicComparability());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getTemporalGranularity(), actual.getTemporalGranularity());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTemporalComparability(), actual.getTemporalComparability());
        assertEquals(expected.getBasePeriod(), actual.getBasePeriod());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getUnitMeasures(), actual.getUnitMeasures());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getStatConcDef(), actual.getStatConcDef());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getStatConcDefLists(), actual.getStatConcDefLists());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getClassSystem(), actual.getClassSystem());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getClassSystemLists(), actual.getClassSystemLists());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getInstanceType(), actual.getInstanceType());
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDocMethod(), actual.getDocMethod());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getSurveySource(), actual.getSurveySource());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getCollMethod(), actual.getCollMethod());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getInformationSuppliers(), actual.getInformationSuppliers());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getFreqColls(), actual.getFreqColls());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDataValidation(), actual.getDataValidation());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDataCompilation(), actual.getDataCompilation());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAdjustment(), actual.getAdjustment());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getCostBurden(), actual.getCostBurden());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getCosts(), actual.getCosts());
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getQualityDoc(), actual.getQualityDoc());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getQualityAssure(), actual.getQualityAssure());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getQualityAssmnt(), actual.getQualityAssmnt());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getUserNeeds(), actual.getUserNeeds());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getUserSat(), actual.getUserSat());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getCompleteness(), actual.getCompleteness());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTimeliness(), actual.getTimeliness());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getPunctuality(), actual.getPunctuality());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAccuracyOverall(), actual.getAccuracyOverall());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getSamplingErr(), actual.getSamplingErr());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getNonsamplingErr(), actual.getNonsamplingErr());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getCoherXDom(), actual.getCoherXDom());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getCoherInternal(), actual.getCoherInternal());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getComment(), actual.getComment());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getNotes(), actual.getNotes());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getParent(), actual.getParent());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getchildren(), actual.getchildren());
    }

    public static void assertEqualsFamiliesNoPagedResult(FamiliesNoPagedResult expecteds, FamiliesNoPagedResult actuals) {
        MetamacRestAsserts.assertEqualsNoPagedResult(expecteds, actuals);
        assertEquals(expecteds.getItems().size(), actuals.getItems().size());
        for (RelatedResource expected : expecteds.getItems()) {
            boolean exists = false;
            for (RelatedResource actual : actuals.getItems()) {
                if (expected.getId().equals(actual.getId())) {
                    exists = true;
                    MetamacRestAsserts.assertEqualsRelatedResource(expected, actual);
                }
            }
            assertTrue(exists);
        }
    }
    
    public static void assertEqualsOperationsPagedResult(OperationsPagedResult expecteds, OperationsPagedResult actuals) {
        MetamacRestAsserts.assertEqualsPagedResult(expecteds, actuals);
        assertEquals(expecteds.getItems().size(), actuals.getItems().size());
        for (RelatedResource expected : expecteds.getItems()) {
            boolean exists = false;
            for (RelatedResource actual : actuals.getItems()) {
                if (expected.getId().equals(actual.getId())) {
                    exists = true;
                    MetamacRestAsserts.assertEqualsRelatedResource(expected, actual);
                }
            }
            assertTrue(exists);
        }
    }
}
