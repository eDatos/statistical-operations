package org.siemac.metamac.statistical.operations.rest.internal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.InternationalString;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.LocalisedString;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public class StatisticalOperationsRestAsserts {

    public static void assertEqualsOperation(Operation expected, Operation actual) {
        assertEquals(expected.getCode(), expected.getCode());
        // TODO uri
        assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        // TODO FAMILY_CODE
        // TODO FAMILY_TITLE
        // TODO SUBJECT_AREA
        // TODO SUBJECT_CODE
        // TODO SECUNDARY_SUBJECT_AREAS
        // TODO SECUNDARY_SUBJECT_CODES
        assertEqualsInternationalString(expected.getObjective(), actual.getObjective());
        assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
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
        assertEqualsInternationalString(expected.getRelPolUsAc(), actual.getRelPolUsAc());
        assertEquals(expected.getRelPolUsAcUrl(), actual.getRelPolUsAcUrl());
        assertEquals(expected.isReleaseCalendar(), actual.isReleaseCalendar());
        assertEquals(expected.getReleaseCalendarAccess(), actual.getReleaseCalendarAccess());
        // TODO UPDATE_FREQUENCY
        // TODO CURRENT_INTERNAL_INSTANCE
        // TODO CURRENT_INSTANCE
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        assertEqualsInternationalString(expected.getRevPolicy(), actual.getRevPolicy());
        assertEquals(expected.getRevPolicyUrl(), actual.getRevPolicyUrl());
        assertEqualsInternationalString(expected.getRevPractice(), actual.getRevPractice());
        assertEquals(expected.getRevPracticeUrl(), actual.getRevPracticeUrl());
        // TODO LEGAL_ACTS
        // TODO DATA_SHARING
        // TODO CONFIDENTIALITY_POLICY
        // TODO CONFIDENTIALITY_DATA_TREATMENT
        assertEqualsInternationalString(expected.getComment(), actual.getComment());
        assertEquals(expected.getCommentUrl(), actual.getCommentUrl());
        assertEqualsInternationalString(expected.getNotes(), actual.getNotes());
        assertEquals(expected.getNotesUrl(), actual.getNotesUrl());
    }

    public static void assertEqualsInternationalString(InternationalString expecteds, InternationalString actuals) {
        if (actuals == null && expecteds == null) {
            return;
        } else if ((actuals != null && expecteds == null) || (actuals == null && expecteds != null)) {
            fail();
        }
        assertEquals(expecteds.getTexts().size(), actuals.getTexts().size());

        for (LocalisedString expected : expecteds.getTexts()) {
            boolean localeExists = false;
            for (LocalisedString actual : actuals.getTexts()) {
                if (expected.getLocale().equals(actual.getLocale())) {
                    assertEquals(expected.getLabel(), actual.getLabel());
                    localeExists = true;
                }
            }
            assertTrue(localeExists);
        }
    }
}
