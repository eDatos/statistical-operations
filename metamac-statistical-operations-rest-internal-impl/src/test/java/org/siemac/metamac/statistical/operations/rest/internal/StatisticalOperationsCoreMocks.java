package org.siemac.metamac.statistical.operations.rest.internal;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.domain.statistical.operations.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;

public class StatisticalOperationsCoreMocks {

    public static Operation mockOperation1() {

        Operation operation = new Operation();

        operation.setCode("Code1");
        operation.setTitle(mockInternationalString("es", "Título 1 en español", "en", "Title 1 in English"));
        operation.setAcronym(mockInternationalString("es", "Acrónimo 1 en español", "en", "Acronym 1 in English"));
        operation.addFamily(mockFamily("familyCode1"));
        operation.addFamily(mockFamily("familyCode2"));
        // TODO SUBJECT_AREA
        // TODO SUBJECT_CODE
        // TODO SECUNDARY_SUBJECT_AREAS
        // TODO SECUNDARY_SUBJECT_CODES
        operation.setObjective(mockInternationalString("es", "Objetivo 1 en español", "en", "Objective 1 in English"));
        operation.setDescription(mockInternationalString("es", "Descripción 1 en español", "en", "Description 1 in English"));
        // TODO INSTANCE_CODE
        // TODO INSTANCE_TITLE

        SurveyType surveyType = new SurveyType();
        surveyType.setDescription(mockInternationalString("es", "Survey 1 en español", "en", "Survey 1 in English"));
        surveyType.setIdentifier("surveyIdentifier");
        operation.setSurveyType(surveyType);

        OfficialityType officialityType = new OfficialityType();
        officialityType.setDescription(mockInternationalString("es", "Officiality 1 en español", "en", "Officiality 1 in English"));
        officialityType.setIdentifier("officialityIdentifier");
        operation.setOfficialityType(officialityType);

        operation.setIndicatorSystem(Boolean.TRUE);
        // TODO PRODUCER
        // TODO REGIONAL_RESPONSIBLE
        // TODO REGIONAL_CONTRIBUTOR
        operation.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0));
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus(StatusEnum.DESIGN);
        operation.setProcStatus(ProcStatusEnum.PUBLISH_EXTERNALLY);
        // TODO PUBLISHER
        operation.setRelPolUsAc(mockInternationalString("es", "RelPolUsAc 1 en español", "en", "RelPolUsAc 1 in English"));
        operation.setRelPolUsAcUrl("http://relPolUsAc1.url");
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        // TODO UPDATE_FREQUENCY
        // TODO CURRENT_INTERNAL_INSTANCE
        // TODO CURRENT_INSTANCE
        operation.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0));
        operation.setRevPolicy(mockInternationalString("es", "RevPolicy 1 en español", "en", "RevPolicy 1 in English"));
        operation.setRevPolicyUrl("http://revPolicy1.url");
        operation.setRevPractice(mockInternationalString("es", "RevPractice 1 en español", "en", "RevPractice 1 in English"));
        operation.setRevPracticeUrl("http://revPractice1.url");
        // TODO LEGAL_ACTS
        // TODO DATA_SHARING
        // TODO CONFIDENTIALITY_POLICY
        // TODO CONFIDENTIALITY_DATA_TREATMENT
        operation.setComment(mockInternationalString("es", "Comentarios 1 en español", "en", "Comments 1 in English"));
        operation.setCommentUrl("http://comments1.url");
        operation.setNotes(mockInternationalString("es", "Notas 1 en español", "en", "Notes 1 in English"));
        operation.setNotesUrl("http://notes1.url");

        return operation;
    }

    private static InternationalString mockInternationalString(String locale1, String label1, String locale2, String label2) {

        InternationalString internationalString = new InternationalString();

        LocalisedString internationalStringLocale1 = new LocalisedString();
        internationalStringLocale1.setLocale(locale1);
        internationalStringLocale1.setLabel(label1);
        internationalString.addText(internationalStringLocale1);

        LocalisedString internationalStringLocale2 = new LocalisedString();
        internationalStringLocale2.setLocale(locale2);
        internationalStringLocale2.setLabel(label2);
        internationalString.addText(internationalStringLocale2);

        return internationalString;
    }

    private static Family mockFamily(String code) {
        Family family = new Family();
        family.setCode(code);
        family.setTitle(mockInternationalString("es", "Título family " + code, "en", "Title family " + code));
        family.setDescription(mockInternationalString("es", "Descripción Familia " + code, "en", "Description Family " + code));
        return family;
    }
}