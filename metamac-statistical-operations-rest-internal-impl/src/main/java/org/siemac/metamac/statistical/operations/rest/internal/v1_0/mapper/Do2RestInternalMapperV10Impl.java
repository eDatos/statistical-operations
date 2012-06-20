package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.rest.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.v1_0.domain.Link;
import org.siemac.metamac.rest.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.v1_0.domain.Resource;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;

@Component
public class Do2RestInternalMapperV10Impl implements Do2RestInternalMapperV10 {

    // @Context
    // private MessageContext context; // Always null in this bean...

    /**
     * TODO
     * <id>""</id>
     * <kind>""</kind>
     * <link rel="self" href="" />
     * <title>""</title>
     * <description>""</description>
     * <parent>
     * <kind>""</kind>
     * <link rel="self" href="" />
     * <title>""</title>
     * </parent>
     * <children>
     * <child>
     * <kind>""</kind>
     * <link rel="self" href="" />
     * <title>""</title>
     * </child>
     * </children>
     */
    @Override
    public Operation toOperation(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl) throws MetamacException {
        if (source == null) {
            return null;
        }
        Operation target = new Operation();
        target.setId(source.getCode());
        target.setLink(toOperationLink(source, apiUrl));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.getFamilies().addAll(toFamiliesResource(source.getFamilies(), apiUrl));
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

    private List<Resource> toFamiliesResource(Set<Family> sources, String apiUrl) {
        List<Resource> targets = new ArrayList<Resource>();
        if (sources == null) {
            return targets;
        }
        for (Family source : sources) {
            Resource target = toFamilyResource(source, apiUrl);
            targets.add(target);
        }
        return targets;
    }

    private Resource toFamilyResource(Family source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_FAMILY);
        target.setLink(toFamilyLink(source, apiUrl));
        target.setTitle(toInternationalString(source.getTitle()));
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

    private Link toOperationLink(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF); 
        link.setHref(createLinkHref(apiUrl, RestInternalConstants.LINK_SUBPATH_OPERATIONS, operation.getCode()));
        return link;
    }

    private Link toFamilyLink(org.siemac.metamac.statistical.operations.core.domain.Family family, String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF); 
        link.setHref(createLinkHref(apiUrl, RestInternalConstants.LINK_SUBPATH_FAMILIES, family.getCode()));
        return link;
    }

    private String createLinkHref(String apiUrl, String baseResource, String resourceId) {
        UriComponentsBuilder uriComponentsBuilder = UriComponentsBuilder.fromHttpUrl(apiUrl);
        uriComponentsBuilder = uriComponentsBuilder.pathSegment(baseResource);
        uriComponentsBuilder = uriComponentsBuilder.pathSegment(resourceId);
        return uriComponentsBuilder.build().toUriString();
    }
}