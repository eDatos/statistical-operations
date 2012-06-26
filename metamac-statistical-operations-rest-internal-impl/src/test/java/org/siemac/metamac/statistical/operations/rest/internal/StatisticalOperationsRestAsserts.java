package org.siemac.metamac.statistical.operations.rest.internal;

import static org.junit.Assert.assertEquals;

import org.siemac.metamac.rest.common.test.utils.MetamacRestAsserts;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public class StatisticalOperationsRestAsserts {

    public static void assertEqualsOperation(Operation expected, Operation actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        MetamacRestAsserts.assertEqualsLink(expected.getLink(), actual.getLink());
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
        assertEquals(expected.getRelPolUsAcUrl(), actual.getRelPolUsAcUrl());
        assertEquals(expected.isReleaseCalendar(), actual.isReleaseCalendar());
        assertEquals(expected.getReleaseCalendarAccess(), actual.getReleaseCalendarAccess());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getUpdateFrequencies(), actual.getUpdateFrequencies());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getCurrentInternalInstance(), actual.getCurrentInternalInstance());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getCurrentInstance(), actual.getCurrentInstance());
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getRevPolicy(), actual.getRevPolicy());
        assertEquals(expected.getRevPolicyUrl(), actual.getRevPolicyUrl());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getRevPractice(), actual.getRevPractice());
        assertEquals(expected.getRevPracticeUrl(), actual.getRevPracticeUrl());
        // TODO LEGAL_ACTS, DATA_SHARING, CONFIDENTIALITY_POLICY, CONFIDENTIALITY_DATA_TREATMENT. No están en OperationBase
        MetamacRestAsserts.assertEqualsInternationalString(expected.getComment(), actual.getComment());
        assertEquals(expected.getCommentUrl(), actual.getCommentUrl());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getNotes(), actual.getNotes());
        assertEquals(expected.getNotesUrl(), actual.getNotesUrl());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getParent(), actual.getParent());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getchildren(), actual.getchildren());
    }
    
    public static void assertEqualsFamily(Family expected, Family actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        MetamacRestAsserts.assertEqualsLink(expected.getLink(), actual.getLink());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        MetamacRestAsserts.assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        MetamacRestAsserts.assertEqualsRelatedResource(expected.getParent(), actual.getParent());
        MetamacRestAsserts.assertEqualsRelatedResources(expected.getchildren(), actual.getchildren());
    }
    
    /**
     * TODO
     * 
CODE
URI
TITLE
ACRONYM
SURVEY_CODE
SURVEY_TITLE
SUCCESSOR
PREDECESSOR
DATA_DESCRIPTION
STATISTICAL_POPULATION
STATISTICAL_UNIT
GEOGRAPHIC_GRANULARITY
GEOGRAPHIC_COMPARABILITY
TEMPORAL_GRANULARITY
TEMPORAL_COMPARABILITY
BASE_PERIOD
UNIT_MEASURE
STAT_CONC_DEF
STAT_CONC_DEF_LIST
CLASS_SYSTEM
CLASS_SYSTEM_LIST
INSTANCE_TYPES
INTERNAL_INVENTORY_DATE
PROC_STATUS
DOC_METHOD
SURVEY_SOURCE
COLL_METHOD
INFORMATION_SUPPLIERS
FREQ_COLL
DATA_VALIDATION
DATA_COMPILATION
ADJUSTMENT
COST_BURDEN
COST
INVENTORY_DATE
QUALITY_DOC
QUALITY_ASSURE
QUALITY_ASSMNT
USER_NEEDS
USER_SAT
COMPLETENESS
TIMELINESS
PUNCTUALITY
ACCURACY_OVERALL
SAMPLING_ERR
NONSAMPLING_ERR
COHER_X_DOM
COHER_INTERNAL
COMMENT
NOTES
        // CHILDREN
        // PARENT

     */

}
