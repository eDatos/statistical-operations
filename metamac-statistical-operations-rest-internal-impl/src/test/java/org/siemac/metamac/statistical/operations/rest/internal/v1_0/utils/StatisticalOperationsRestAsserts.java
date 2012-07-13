package org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils;

import static org.junit.Assert.assertEquals;

import org.siemac.metamac.rest.common.test.utils.MetamacRestAsserts;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public class StatisticalOperationsRestAsserts {

    public static void assertEqualsOperation(Operation expected, Operation actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getUrn(), actual.getUrn());
        assertEquals(expected.getKind(), actual.getKind());
        assertEquals(expected.getSelfLink(), actual.getSelfLink());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        MetamacRestAsserts.assertEqualsResources(expected.getFamilies(), actual.getFamilies());
        MetamacRestAsserts.assertEqualsResource(expected.getSubjectArea(), actual.getSubjectArea());
        MetamacRestAsserts.assertEqualsResources(expected.getSecondarySubjectAreas(), actual.getSecondarySubjectAreas());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getObjective(), actual.getObjective());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
        MetamacRestAsserts.assertEqualsResources(expected.getInstances(), actual.getInstances());
        MetamacRestAsserts.assertEqualsMetadataComplexValue(expected.getSurveyType(), actual.getSurveyType());
        MetamacRestAsserts.assertEqualsMetadataComplexValue(expected.getOfficialityType(), actual.getOfficialityType());
        assertEquals(expected.isIndicatorSystem(), actual.isIndicatorSystem());
        MetamacRestAsserts.assertEqualsResources(expected.getProducers(), actual.getProducers());
        MetamacRestAsserts.assertEqualsResources(expected.getRegionalResponsibles(), actual.getRegionalResponsibles());
        MetamacRestAsserts.assertEqualsResources(expected.getRegionalContributors(), actual.getRegionalContributors());
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.isCurrentlyActive(), actual.isCurrentlyActive());
        assertEquals(expected.getStatus(), actual.getStatus());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        MetamacRestAsserts.assertEqualsResources(expected.getPublishers(), actual.getPublishers());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getRelPolUsAc(), actual.getRelPolUsAc());
        assertEquals(expected.isReleaseCalendar(), actual.isReleaseCalendar());
        assertEquals(expected.getReleaseCalendarAccess(), actual.getReleaseCalendarAccess());
        MetamacRestAsserts.assertEqualsResources(expected.getUpdateFrequencies(), actual.getUpdateFrequencies());
        MetamacRestAsserts.assertEqualsResource(expected.getCurrentInternalInstance(), actual.getCurrentInternalInstance());
        MetamacRestAsserts.assertEqualsResource(expected.getCurrentInstance(), actual.getCurrentInstance());
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getRevPolicy(), actual.getRevPolicy());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getRevPractice(), actual.getRevPractice());
        MetamacRestAsserts.assertEqualsResources(expected.getContacts(), actual.getContacts());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getLegalActs(), actual.getLegalActs());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDataSharing(), actual.getDataSharing());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getConfidentialityPolicy(), actual.getConfidentialityPolicy());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getConfidentialityDataTreatment(), actual.getConfidentialityDataTreatment());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getNotes(), actual.getNotes());
        MetamacRestAsserts.assertEqualsResourceLink(expected.getParent(), actual.getParent());
        MetamacRestAsserts.assertEqualsResourcesLinks(expected.getchildren(), actual.getchildren());
    }

    public static void assertEqualsFamily(Family expected, Family actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getUrn(), actual.getUrn());
        assertEquals(expected.getKind(), actual.getKind());
        assertEquals(expected.getSelfLink(), actual.getSelfLink());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        MetamacRestAsserts.assertEqualsResourceLink(expected.getParent(), actual.getParent());
        MetamacRestAsserts.assertEqualsResourcesLinks(expected.getchildren(), actual.getchildren());
    }

    public static void assertEqualsInstance(Instance expected, Instance actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getUrn(), actual.getUrn());
        assertEquals(expected.getKind(), actual.getKind());
        assertEquals(expected.getSelfLink(), actual.getSelfLink());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        MetamacRestAsserts.assertEqualsResource(expected.getSurvey(), actual.getSurvey());
        MetamacRestAsserts.assertEqualsResource(expected.getSuccessor(), actual.getSuccessor());
        MetamacRestAsserts.assertEqualsResource(expected.getPredecessor(), actual.getPredecessor());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDataDescription(), actual.getDataDescription());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getStatisticalPopulation(), actual.getStatisticalPopulation());
        MetamacRestAsserts.assertEqualsResources(expected.getStatisticalUnits(), actual.getStatisticalUnits());
        MetamacRestAsserts.assertEqualsResource(expected.getGeographicGranularity(), actual.getGeographicGranularity());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getGeographicComparability(), actual.getGeographicComparability());
        MetamacRestAsserts.assertEqualsResource(expected.getTemporalGranularity(), actual.getTemporalGranularity());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTemporalComparability(), actual.getTemporalComparability());
        assertEquals(expected.getBasePeriod(), actual.getBasePeriod());
        MetamacRestAsserts.assertEqualsResources(expected.getUnitMeasures(), actual.getUnitMeasures());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getStatConcDef(), actual.getStatConcDef());
        MetamacRestAsserts.assertEqualsResources(expected.getStatConcDefLists(), actual.getStatConcDefLists());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getClassSystem(), actual.getClassSystem());
        MetamacRestAsserts.assertEqualsResources(expected.getClassSystemLists(), actual.getClassSystemLists());
        MetamacRestAsserts.assertEqualsMetadataComplexValue(expected.getInstanceType(), actual.getInstanceType());
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDocMethod(), actual.getDocMethod());
        MetamacRestAsserts.assertEqualsMetadataComplexValue(expected.getSurveySource(), actual.getSurveySource());
        MetamacRestAsserts.assertEqualsMetadataComplexValue(expected.getCollMethod(), actual.getCollMethod());
        MetamacRestAsserts.assertEqualsResources(expected.getInformationSuppliers(), actual.getInformationSuppliers());
        MetamacRestAsserts.assertEqualsResources(expected.getFreqColls(), actual.getFreqColls());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDataValidation(), actual.getDataValidation());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDataCompilation(), actual.getDataCompilation());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAdjustment(), actual.getAdjustment());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getCostBurden(), actual.getCostBurden());
        MetamacRestAsserts.assertEqualsMetadataComplexValues(expected.getCosts(), actual.getCosts());
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
        MetamacRestAsserts.assertEqualsResource(expected.getParent(), actual.getParent());
        MetamacRestAsserts.assertEqualsResourcesLinks(expected.getchildren(), actual.getchildren());
    }
}
