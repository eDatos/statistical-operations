package org.siemac.metamac.statistical.operations.rest.internal;

import static org.junit.Assert.assertEquals;

import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public class StatisticalOperationsRestAsserts {

    // TODO parent, kind, children
    public static void assertEqualsOperation(Operation expected, Operation actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        RestAsserts.assertEqualsLink(expected.getLink(), actual.getLink());
        RestAsserts.assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        RestAsserts.assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        RestAsserts.assertEqualsResources(expected.getFamilies(), actual.getFamilies());
        // TODO SUBJECT_AREA
        // TODO SUBJECT_CODE
        // TODO SECUNDARY_SUBJECT_AREAS
        // TODO SECUNDARY_SUBJECT_CODES
        RestAsserts.assertEqualsInternationalString(expected.getObjective(), actual.getObjective());
        RestAsserts.assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
        // TODO INSTANCE_CODE
        // TODO INSTANCE_TITLE
        // TODO survey type: lista? mirar excel
        assertEquals(expected.getSurveyType(), actual.getSurveyType());
        // TODO officialy type: lista? mirar excel
        assertEquals(expected.getOfficialityType(), actual.getOfficialityType());
        assertEquals(expected.isIndicatorSystem(), actual.isIndicatorSystem());
        // TODO PRODUCER
        // TODO REGIONAL_RESPONSIBLE
        // TODO REGIONAL_CONTRIBUTOR
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.isCurrentlyActive(), actual.isCurrentlyActive());
        assertEquals(expected.getStatus(), actual.getStatus());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        // TODO PUBLISHER
        RestAsserts.assertEqualsInternationalString(expected.getRelPolUsAc(), actual.getRelPolUsAc());
        assertEquals(expected.getRelPolUsAcUrl(), actual.getRelPolUsAcUrl());
        assertEquals(expected.isReleaseCalendar(), actual.isReleaseCalendar());
        assertEquals(expected.getReleaseCalendarAccess(), actual.getReleaseCalendarAccess());
        // TODO UPDATE_FREQUENCY
        // TODO CURRENT_INTERNAL_INSTANCE
        // TODO CURRENT_INSTANCE
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        RestAsserts.assertEqualsInternationalString(expected.getRevPolicy(), actual.getRevPolicy());
        assertEquals(expected.getRevPolicyUrl(), actual.getRevPolicyUrl());
        RestAsserts.assertEqualsInternationalString(expected.getRevPractice(), actual.getRevPractice());
        assertEquals(expected.getRevPracticeUrl(), actual.getRevPracticeUrl());
        // TODO LEGAL_ACTS
        // TODO DATA_SHARING
        // TODO CONFIDENTIALITY_POLICY
        // TODO CONFIDENTIALITY_DATA_TREATMENT
        RestAsserts.assertEqualsInternationalString(expected.getComment(), actual.getComment());
        assertEquals(expected.getCommentUrl(), actual.getCommentUrl());
        RestAsserts.assertEqualsInternationalString(expected.getNotes(), actual.getNotes());
        assertEquals(expected.getNotesUrl(), actual.getNotesUrl());
    }}
