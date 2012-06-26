package org.siemac.metamac.statistical.operations.rest.internal;

import static org.junit.Assert.assertEquals;

import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public class StatisticalOperationsRestAsserts {

    public static void assertEqualsOperation(Operation expected, Operation actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        RestAsserts.assertEqualsLink(expected.getLink(), actual.getLink());
        RestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        RestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        RestAsserts.assertEqualsRelatedResources(expected.getFamilies(), actual.getFamilies());
        RestAsserts.assertEqualsRelatedResource(expected.getSubjectArea(), actual.getSubjectArea());
        RestAsserts.assertEqualsRelatedResources(expected.getSecondarySubjectAreas(), actual.getSecondarySubjectAreas());
        RestAsserts.assertEqualsInternationalString(expected.getObjective(), actual.getObjective());
        RestAsserts.assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
        RestAsserts.assertEqualsRelatedResources(expected.getInstances(), actual.getInstances());
        RestAsserts.assertEqualsRelatedResource(expected.getSurveyType(), actual.getSurveyType());
        RestAsserts.assertEqualsRelatedResource(expected.getOfficialityType(), actual.getOfficialityType());
        assertEquals(expected.isIndicatorSystem(), actual.isIndicatorSystem());
        RestAsserts.assertEqualsRelatedResources(expected.getProducers(), actual.getProducers());
        RestAsserts.assertEqualsRelatedResources(expected.getRegionalResponsibles(), actual.getRegionalResponsibles());
        RestAsserts.assertEqualsRelatedResources(expected.getRegionalContributors(), actual.getRegionalContributors());
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.isCurrentlyActive(), actual.isCurrentlyActive());
        assertEquals(expected.getStatus(), actual.getStatus());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        RestAsserts.assertEqualsRelatedResources(expected.getPublishers(), actual.getPublishers());
        RestAsserts.assertEqualsInternationalString(expected.getRelPolUsAc(), actual.getRelPolUsAc());
        assertEquals(expected.getRelPolUsAcUrl(), actual.getRelPolUsAcUrl());
        assertEquals(expected.isReleaseCalendar(), actual.isReleaseCalendar());
        assertEquals(expected.getReleaseCalendarAccess(), actual.getReleaseCalendarAccess());
        RestAsserts.assertEqualsRelatedResources(expected.getUpdateFrequencies(), actual.getUpdateFrequencies());
        RestAsserts.assertEqualsRelatedResource(expected.getCurrentInternalInstance(), actual.getCurrentInternalInstance());
        RestAsserts.assertEqualsRelatedResource(expected.getCurrentInstance(), actual.getCurrentInstance());
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        RestAsserts.assertEqualsInternationalString(expected.getRevPolicy(), actual.getRevPolicy());
        assertEquals(expected.getRevPolicyUrl(), actual.getRevPolicyUrl());
        RestAsserts.assertEqualsInternationalString(expected.getRevPractice(), actual.getRevPractice());
        assertEquals(expected.getRevPracticeUrl(), actual.getRevPracticeUrl());
        // TODO LEGAL_ACTS, DATA_SHARING, CONFIDENTIALITY_POLICY, CONFIDENTIALITY_DATA_TREATMENT. No están en OperationBase
        RestAsserts.assertEqualsInternationalString(expected.getComment(), actual.getComment());
        assertEquals(expected.getCommentUrl(), actual.getCommentUrl());
        RestAsserts.assertEqualsInternationalString(expected.getNotes(), actual.getNotes());
        assertEquals(expected.getNotesUrl(), actual.getNotesUrl());
        RestAsserts.assertEqualsRelatedResource(expected.getParent(), actual.getParent());
        RestAsserts.assertEqualsRelatedResources(expected.getchildren(), actual.getchildren());
    }
    
    public static void assertEqualsFamily(Family expected, Family actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        RestAsserts.assertEqualsLink(expected.getLink(), actual.getLink());
        RestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        RestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        RestAsserts.assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        RestAsserts.assertEqualsRelatedResource(expected.getParent(), actual.getParent());
        RestAsserts.assertEqualsRelatedResources(expected.getchildren(), actual.getchildren());
    }
}
