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
        RestAsserts.assertEqualsResources(expected.getFamilies(), actual.getFamilies());
        RestAsserts.assertEqualsResource(expected.getSubjectArea(), actual.getSubjectArea());
        RestAsserts.assertEqualsResources(expected.getSecondarySubjectAreas(), actual.getSecondarySubjectAreas());
        RestAsserts.assertEqualsInternationalString(expected.getObjective(), actual.getObjective());
        RestAsserts.assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
        RestAsserts.assertEqualsResources(expected.getInstances(), actual.getInstances());
        RestAsserts.assertEqualsResource(expected.getSurveyType(), actual.getSurveyType());
        RestAsserts.assertEqualsResource(expected.getOfficialityType(), actual.getOfficialityType());
        assertEquals(expected.isIndicatorSystem(), actual.isIndicatorSystem());
        RestAsserts.assertEqualsResources(expected.getProducers(), actual.getProducers());
        RestAsserts.assertEqualsResources(expected.getRegionalResponsibles(), actual.getRegionalResponsibles());
        RestAsserts.assertEqualsResources(expected.getRegionalContributors(), actual.getRegionalContributors());
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.isCurrentlyActive(), actual.isCurrentlyActive());
        assertEquals(expected.getStatus(), actual.getStatus());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        RestAsserts.assertEqualsResources(expected.getPublishers(), actual.getPublishers());
        RestAsserts.assertEqualsInternationalString(expected.getRelPolUsAc(), actual.getRelPolUsAc());
        assertEquals(expected.getRelPolUsAcUrl(), actual.getRelPolUsAcUrl());
        assertEquals(expected.isReleaseCalendar(), actual.isReleaseCalendar());
        assertEquals(expected.getReleaseCalendarAccess(), actual.getReleaseCalendarAccess());
        RestAsserts.assertEqualsResources(expected.getUpdateFrequencies(), actual.getUpdateFrequencies());
        RestAsserts.assertEqualsResource(expected.getCurrentInternalInstance(), actual.getCurrentInternalInstance());
        RestAsserts.assertEqualsResource(expected.getCurrentInstance(), actual.getCurrentInstance());
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
        RestAsserts.assertEqualsResource(expected.getParent(), actual.getParent());
        RestAsserts.assertEqualsResources(expected.getchildren(), actual.getchildren());
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
        RestAsserts.assertEqualsResource(expected.getParent(), actual.getParent());
        RestAsserts.assertEqualsResources(expected.getchildren(), actual.getchildren());
    }
}
