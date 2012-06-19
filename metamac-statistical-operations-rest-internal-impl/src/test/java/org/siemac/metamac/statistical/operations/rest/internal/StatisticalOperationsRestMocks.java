package org.siemac.metamac.statistical.operations.rest.internal;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.LocalisedString;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;


public class StatisticalOperationsRestMocks {

    public static Operation mockOperation1() {

        Operation operation = new Operation();

        operation.setCode("Code1");
        operation.getTitles().addAll(mockInternationalString("es", "Título en español de operación", "en", "Operation title in English"));

        // // DESCRIPTION
        // InternationalString acronym = new InternationalString();
        // LocalisedString acronym_es = new LocalisedString();
        // acronym_es.setLabel("Descripción en español de operacion");
        // acronym_es.setLocale("es");
        // LocalisedString acronym_en = new LocalisedString();
        // acronym_en.setLabel("Descripción en inglés de operacion");
        // acronym_en.setLocale("en");
        // acronym.addText(acronym_es);
        // acronym.addText(acronym_en);
        // operation.setAcronym(acronym);
        //
        // // RELEASE_CALENDAR
        // operation.setReleaseCalendar(false);
        //
        // // RELEASE_CALENDAR_ACCESS
        // operation.setReleaseCalendarAccess("http://www.draft.com");
        //
        // // SURVEY_TYPE
        // operation.setSurveyType(statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextAdministrador(), Long.valueOf(1)));
        //
        // // OFFICIALITY_TYPE
        // operation.setOfficialityType(statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContextAdministrador(), Long.valueOf(1)));
        //
        // // SUBJECT_AREA
        // ExternalItemBt subjectArea = new ExternalItemBt();
        // subjectArea.setCodeId("PRUEBA");
        // subjectArea.setType(TypeExternalArtefactsEnum.CATEGORY);
        // subjectArea.setUriInt("uri:external:todo");
        // operation.setSubjectArea(subjectArea);
        //
        // // STATUS
        // operation.setStatus(StatusEnum.PLANNING);
        //
        // // INDICATOR_SYSTEM
        // operation.setIndicatorSystem(true);
        //
        //
        // // OBJECTIVE
        // InternationalString objective = new InternationalString();
        // LocalisedString objective_es = new LocalisedString();
        // objective_es.setLabel("OPERACION - OBJECTIVE - ES");
        // objective_es.setLocale("es");
        // LocalisedString objective_en = new LocalisedString();
        // objective_en.setLabel("OPERACION - OBJECTIVE - EN");
        // objective_en.setLocale("en");
        // objective.addText(objective_es);
        // objective.addText(objective_en);
        // operation.setObjective(objective);
        //
        // // SURVEY_TYPE
        // operation.setSurveyType(statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContextAdministrador(), Long.valueOf(1)));
        //
        // // PRODUCER
        // ExternalItemBt producer01 = new ExternalItemBt();
        // producer01.setCodeId("ISTAC");
        // producer01.setType(TypeExternalArtefactsEnum.AGENCY);
        // producer01.setUriInt("uri:interna:todo");
        // operation.addProducer(producer01);
        //
        // ExternalItemBt producer02 = new ExternalItemBt();
        // producer02.setCodeId("INE");
        // producer02.setType(TypeExternalArtefactsEnum.AGENCY);
        // producer02.setUriInt("uri:interna:todo");
        // operation.addProducer(producer02);
        //
        // // REGIONAL_RESPONSIBLE
        // ExternalItemBt regionalResponsible01 = new ExternalItemBt();
        // regionalResponsible01.setCodeId("ISTAC");
        // regionalResponsible01.setType(TypeExternalArtefactsEnum.AGENCY);
        // regionalResponsible01.setUriInt("uri:interna:todo");
        // operation.addRegionalResponsible(regionalResponsible01);
        //
        // // PUBLISHER
        // ExternalItemBt publisher01 = new ExternalItemBt();
        // publisher01.setCodeId("ISTAC");
        // publisher01.setType(TypeExternalArtefactsEnum.AGENCY);
        // publisher01.setUriInt("uri:interna:todo");
        // operation.addPublisher(publisher01);
        //
        // // COMMON_METADATA
        // ExternalItemBt commonMetadata = new ExternalItemBt();
        // commonMetadata.setCodeId("ISTAC");
        // commonMetadata.setType(TypeExternalArtefactsEnum.AGENCY);
        // commonMetadata.setUriInt("uri:interna:todo");
        // operation.setCommonMetadata(commonMetadata);

        return operation;
    }

    private static List<LocalisedString> mockInternationalString(String locale1, String label1, String locale2, String label2) {

        List<LocalisedString> internationalString = new ArrayList<LocalisedString>();
        LocalisedString internationalStringLocale1 = new LocalisedString();
        internationalStringLocale1.setLocale(locale1);
        internationalStringLocale1.setLabel(label1);
        internationalString.add(internationalStringLocale1);

        LocalisedString internationalStringLocale2 = new LocalisedString();
        internationalStringLocale2.setLocale(locale2);
        internationalStringLocale2.setLabel(label2);
        internationalString.add(internationalStringLocale2);

        return internationalString;
    }
}
