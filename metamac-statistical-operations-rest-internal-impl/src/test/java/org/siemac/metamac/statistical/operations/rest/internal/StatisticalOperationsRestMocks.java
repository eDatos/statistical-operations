package org.siemac.metamac.statistical.operations.rest.internal;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public class StatisticalOperationsRestMocks {

    public static Operation mockOperation1(String baseApi) {

        Operation operation = new Operation();

        operation.setId("Code1");
        operation.setKind(RestInternalConstants.KIND_OPERATION);
        operation.setLink(RestMocks.mockLink(RestInternalConstants.LINK_SELF, baseApi + "/operations/Code1"));
        operation.setTitle(RestMocks.mockInternationalString("es", "Título 1 en español", "en", "Title 1 in English"));
        operation.setAcronym(RestMocks.mockInternationalString("es", "Acrónimo 1 en español", "en", "Acronym 1 in English"));
        operation.getFamilies().add(mockFamily("familyCode1", baseApi));
        operation.getFamilies().add(mockFamily("familyCode2", baseApi));
        operation.setSubjectArea(RestMocks.mockResource("subjectArea1", TypeExternalArtefactsEnum.CATEGORY.name(), RestInternalConstants.LINK_SELF, "http://subjectArea1", Boolean.FALSE, null));
        // TODO SECUNDARY_SUBJECT_AREAS
        // TODO SECUNDARY_SUBJECT_CODES
        operation.setObjective(RestMocks.mockInternationalString("es", "Objetivo 1 en español", "en", "Objective 1 in English"));
        operation.setDescription(RestMocks.mockInternationalString("es", "Descripción 1 en español", "en", "Description 1 in English"));
        // TODO INSTANCE_CODE
        // TODO INSTANCE_TITLE
        operation.setSurveyType("surveyIdentifier");
        operation.setOfficialityType("officialityIdentifier");

        operation.setIndicatorSystem(Boolean.TRUE);
        // TODO PRODUCER
        // TODO REGIONAL_RESPONSIBLE
        // TODO REGIONAL_CONTRIBUTOR
        operation.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus("DESIGN");
        operation.setProcStatus("PUBLISH_EXTERNALLY");
        // TODO PUBLISHER
        operation.setRelPolUsAc(RestMocks.mockInternationalString("es", "RelPolUsAc 1 en español", "en", "RelPolUsAc 1 in English"));
        operation.setRelPolUsAcUrl("http://relPolUsAc1.url");
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        // TODO UPDATE_FREQUENCY
        // TODO CURRENT_INTERNAL_INSTANCE
        // TODO CURRENT_INSTANCE
        operation.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        operation.setRevPolicy(RestMocks.mockInternationalString("es", "RevPolicy 1 en español", "en", "RevPolicy 1 in English"));
        operation.setRevPolicyUrl("http://revPolicy1.url");
        operation.setRevPractice(RestMocks.mockInternationalString("es", "RevPractice 1 en español", "en", "RevPractice 1 in English"));
        operation.setRevPracticeUrl("http://revPractice1.url");
        // TODO LEGAL_ACTS
        // TODO DATA_SHARING
        // TODO CONFIDENTIALITY_POLICY
        // TODO CONFIDENTIALITY_DATA_TREATMENT
        operation.setComment(RestMocks.mockInternationalString("es", "Comentarios 1 en español", "en", "Comments 1 in English"));
        operation.setCommentUrl("http://comments1.url");
        operation.setNotes(RestMocks.mockInternationalString("es", "Notas 1 en español", "en", "Notes 1 in English"));
        operation.setNotesUrl("http://notes1.url");

        return operation;
    }

    private static Resource mockFamily(String id, String baseApi) {
        return RestMocks.mockResource(id, RestInternalConstants.KIND_FAMILY, RestInternalConstants.LINK_SELF, baseApi + "/families/" + id, Boolean.TRUE, "family");
    }
}
