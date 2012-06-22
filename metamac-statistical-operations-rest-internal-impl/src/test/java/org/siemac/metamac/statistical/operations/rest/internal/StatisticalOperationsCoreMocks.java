package org.siemac.metamac.statistical.operations.rest.internal;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.bt.domain.ExternalItemBt;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.vo.domain.ExternalItem;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.domain.statistical.operations.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
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
        operation.setSubjectArea(mockExternalItemBt("subjectArea1", TypeExternalArtefactsEnum.CATEGORY, "http://subjectArea1"));
        operation.getSecondarySubjectAreas().add(mockExternalItem("secundarySubjectArea1", TypeExternalArtefactsEnum.CATEGORY, "http://secundarySubjectArea1"));
        operation.getSecondarySubjectAreas().add(mockExternalItem("secundarySubjectArea2", TypeExternalArtefactsEnum.CATEGORY, "http://secundarySubjectArea2"));
        operation.getSecondarySubjectAreas().add(mockExternalItem("secundarySubjectArea3", TypeExternalArtefactsEnum.CATEGORY, "http://secundarySubjectArea3"));
        operation.setObjective(mockInternationalString("es", "Objetivo 1 en español", "en", "Objective 1 in English"));
        operation.setDescription(mockInternationalString("es", "Descripción 1 en español", "en", "Description 1 in English"));
        operation.addInstance(mockInstance("instanceCode1"));
        operation.addInstance(mockInstance("instanceCode2"));
        operation.addInstance(mockInstance("instanceCode3"));
        operation.setSurveyType(mockSurveyType("surveyIdentifier"));
        operation.setOfficialityType(mockOfficialityType("officialityIdentifier"));
        operation.setIndicatorSystem(Boolean.TRUE);
        operation.getProducer().add(mockExternalItem("producer1", TypeExternalArtefactsEnum.AGENCY, "http://producer1"));
        operation.getProducer().add(mockExternalItem("producer2", TypeExternalArtefactsEnum.AGENCY, "http://producer2"));
        operation.getRegionalResponsible().add(mockExternalItem("regionalResponsible1", TypeExternalArtefactsEnum.AGENCY, "http://regionalResponsible1"));
        operation.getRegionalResponsible().add(mockExternalItem("regionalResponsible2", TypeExternalArtefactsEnum.AGENCY, "http://regionalResponsible2"));
        operation.getRegionalResponsible().add(mockExternalItem("regionalResponsible3", TypeExternalArtefactsEnum.AGENCY, "http://regionalResponsible3"));
        operation.getRegionalContributor().add(mockExternalItem("regionalContributor1", TypeExternalArtefactsEnum.AGENCY, "http://regionalContributor1"));
        operation.getRegionalContributor().add(mockExternalItem("regionalContributor2", TypeExternalArtefactsEnum.AGENCY, "http://regionalContributor2"));
        operation.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0));
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus(StatusEnum.DESIGN);
        operation.setProcStatus(ProcStatusEnum.PUBLISH_EXTERNALLY);
        operation.getPublisher().add(mockExternalItem("publisher1", TypeExternalArtefactsEnum.AGENCY, "http://publisher1"));
        operation.getPublisher().add(mockExternalItem("publisher2", TypeExternalArtefactsEnum.AGENCY, "http://publisher2"));
        operation.getPublisher().add(mockExternalItem("publisher3", TypeExternalArtefactsEnum.AGENCY, "http://publisher3"));
        operation.setRelPolUsAc(mockInternationalString("es", "RelPolUsAc 1 en español", "en", "RelPolUsAc 1 in English"));
        operation.setRelPolUsAcUrl("http://relPolUsAc1.url");
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency1", TypeExternalArtefactsEnum.CODE, "http://updateFrequency1"));
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency2", TypeExternalArtefactsEnum.CODE, "http://updateFrequency2"));
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency3", TypeExternalArtefactsEnum.CODE, "http://updateFrequency3"));
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency4", TypeExternalArtefactsEnum.CODE, "http://updateFrequency4"));
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

    private static Instance mockInstance(String code) {
        Instance instance = new Instance();
        instance.setCode(code);
        instance.setTitle(mockInternationalString("es", "Título instance " + code, "en", "Title instance " + code));
        return instance;
    }

    private static SurveyType mockSurveyType(String code) {
        SurveyType surveyType = new SurveyType();
        surveyType.setDescription(mockInternationalString("es", "Título survey " + code, "en", "Title survey " + code));
        surveyType.setIdentifier(code);
        return surveyType;
    }

    private static OfficialityType mockOfficialityType(String code) {
        OfficialityType officialityType = new OfficialityType();
        officialityType.setDescription(mockInternationalString("es", "Título officialityType " + code, "en", "Title officialityType " + code));
        officialityType.setIdentifier(code);
        return officialityType;
    }

    private static ExternalItemBt mockExternalItemBt(String code, TypeExternalArtefactsEnum type, String uri) {
        return new ExternalItemBt(uri, code, type);
    }

    private static ExternalItem mockExternalItem(String code, TypeExternalArtefactsEnum type, String uri) {
        return new ExternalItem(new ExternalItemBt(uri, code, type));
    }
}