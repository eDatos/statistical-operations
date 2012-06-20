package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.util.Date;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.InternationalString;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.LocalisedString;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.springframework.stereotype.Component;

@Component
public class Do2RestInternalMapperImpl implements Do2RestInternalMapper {

//    @Context
//    private MessageContext context;     // Always null in this bean...

    // TODO conversión de DateTime de Joda ¿qué mostrar en el xml?
    @Override
    public Operation toOperation(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl) throws MetamacException {
        if (source == null) {
            return null;
        }
        Operation target = new Operation();
        target.setCode(source.getCode());
        target.setSelfLink(createUrlOperationsOperation(source, apiUrl));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        // TODO FAMILY_CODE
        // TODO FAMILY_TITLE
        // TODO SUBJECT_AREA
        // TODO SUBJECT_CODE
        // TODO SECUNDARY_SUBJECT_AREAS
        // TODO SECUNDARY_SUBJECT_CODES
        target.setObjective(toInternationalString(source.getObjective()));
        target.setDescription(toInternationalString(source.getDescription()));
        // TODO INSTANCE_CODE
        // TODO INSTANCE_TITLE
        // TODO SURVEY_TYPE: qué dato?
        if (source.getSurveyType() != null) {
            target.setSurveyType(source.getSurveyType().getIdentifier());
        }
        // TODO officialy type: qué dato?
        if (source.getOfficialityType() != null) {
            target.setOfficialityType(source.getOfficialityType().getIdentifier());
        }
        target.setIndicatorSystem(source.getIndicatorSystem());
        // TODO PRODUCER
        // TODO REGIONAL_RESPONSIBLE
        // TODO REGIONAL_CONTRIBUTOR
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setCurrentlyActive(source.getCurrentlyActive());
        // TODO status, qué poner
        if (source.getStatus() != null) {
            target.setStatus(source.getStatus().name());
        }
        // TODO proc status, qué poner
        target.setProcStatus(source.getProcStatus().name());
        // TODO PUBLISHER
        target.setRelPolUsAc(toInternationalString(source.getRelPolUsAc()));
        target.setRelPolUsAcUrl(source.getRelPolUsAcUrl());
        target.setReleaseCalendar(source.getReleaseCalendar());
        target.setReleaseCalendarAccess(source.getReleaseCalendarAccess());
        // TODO UPDATE_FREQUENCY
        // TODO CURRENT_INTERNAL_INSTANCE
        // TODO CURRENT_INSTANCE
        target.setInventoryDate(toDate(source.getInventoryDate()));
        target.setRevPolicy(toInternationalString(source.getRevPolicy()));
        target.setRevPolicyUrl(source.getRevPolicyUrl());
        target.setRevPractice(toInternationalString(source.getRevPractice()));
        target.setRevPracticeUrl(source.getRevPracticeUrl());
        // TODO LEGAL_ACTS
        // TODO DATA_SHARING
        // TODO CONFIDENTIALITY_POLICY
        // TODO CONFIDENTIALITY_DATA_TREATMENT
        target.setComment(toInternationalString(source.getComment()));
        target.setCommentUrl(source.getCommentUrl());
        target.setNotes(toInternationalString(source.getNotes()));
        target.setNotesUrl(source.getNotesUrl());

        return target;
    }

    private InternationalString toInternationalString(org.siemac.metamac.core.common.ent.domain.InternationalString sources) {
        if (sources == null) {
            return null;
        }
        InternationalString targets = new InternationalString();
        for (org.siemac.metamac.core.common.ent.domain.LocalisedString source : sources.getTexts()) {
            LocalisedString target = new LocalisedString();
            target.setLabel(source.getLabel());
            target.setLocale(source.getLocale());
            targets.getTexts().add(target);
        }
        return targets;
    }

    private Date toDate(DateTime source) {
        if (source == null) {
            return null;
        }
        return source.toDate();
    }
    

    private String createUrlOperationsOperation(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        StringBuilder url = new StringBuilder();
        url.append(apiUrl);
        url.append("/operations/" + operation.getCode());
        return url.toString();
    }
}
