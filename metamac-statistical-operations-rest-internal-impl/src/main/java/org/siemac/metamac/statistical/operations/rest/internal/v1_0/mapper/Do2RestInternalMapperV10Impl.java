package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.bt.domain.ExternalItemBt;
import org.siemac.metamac.core.common.exception.CommonServiceExceptionType;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItem;
import org.siemac.metamac.core.common.vo.domain.ExternalItem;
import org.siemac.metamac.rest.common.v1_0.domain.ErrorItem;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.Link;
import org.siemac.metamac.rest.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;

@Component
public class Do2RestInternalMapperV10Impl implements Do2RestInternalMapperV10 {

    // @Context
    // private MessageContext context; // Always null in this bean (not in Service)...

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
    public Operation toOperation(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Operation target = new Operation();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_OPERATION);
        target.setLink(toOperationLink(source, apiUrl));
        target.setTitle(toInternationalString(source.getTitle()));
        target.setAcronym(toInternationalString(source.getAcronym()));
        target.getFamilies().addAll(familiesToResources(source.getFamilies(), apiUrl));
        target.setSubjectArea(externalItemToResource(source.getSubjectArea()));
        target.getSecondarySubjectAreas().addAll(externalItemsToResources(source.getSecondarySubjectAreas()));
        target.setObjective(toInternationalString(source.getObjective()));
        target.setDescription(toInternationalString(source.getDescription()));
        target.getInstances().addAll(instancesToResources(source.getInstances(), apiUrl));
        if (source.getSurveyType() != null) {
            // TODO SURVEY_TYPE: qué dato?
            target.setSurveyType(source.getSurveyType().getIdentifier());
        }
        if (source.getOfficialityType() != null) {
            // TODO officialy type: qué dato?
            target.setOfficialityType(source.getOfficialityType().getIdentifier());
        }
        target.setIndicatorSystem(source.getIndicatorSystem());
        target.getProducers().addAll(externalItemsToResources(source.getProducer()));
        target.getRegionalResponsibles().addAll(externalItemsToResources(source.getRegionalResponsible()));
        target.getRegionalContributors().addAll(externalItemsToResources(source.getRegionalContributor()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setCurrentlyActive(source.getCurrentlyActive());
        if (source.getStatus() != null) {
            target.setStatus(source.getStatus().name());
        }
        target.setProcStatus(source.getProcStatus().name());
        target.getPublishers().addAll(externalItemsToResources(source.getPublisher()));
        target.setRelPolUsAc(toInternationalString(source.getRelPolUsAc()));
        target.setRelPolUsAcUrl(source.getRelPolUsAcUrl());
        target.setReleaseCalendar(source.getReleaseCalendar());
        target.setReleaseCalendarAccess(source.getReleaseCalendarAccess());
        target.getUpdateFrequencies().addAll(externalItemsToResources(source.getUpdateFrequency()));
        // TODO CURRENT_INTERNAL_INSTANCE No está en OperationBase
        // TODO CURRENT_INSTANCE No está en OperationBase
        target.setInventoryDate(toDate(source.getInventoryDate()));
        target.setRevPolicy(toInternationalString(source.getRevPolicy()));
        target.setRevPolicyUrl(source.getRevPolicyUrl());
        target.setRevPractice(toInternationalString(source.getRevPractice()));
        target.setRevPracticeUrl(source.getRevPracticeUrl());
        // TODO LEGAL_ACTS No está en OperationBase
        // TODO DATA_SHARING No está en OperationBase
        // TODO CONFIDENTIALITY_POLICY No está en OperationBase
        // TODO CONFIDENTIALITY_DATA_TREATMENT No está en OperationBase
        target.setComment(toInternationalString(source.getComment()));
        target.setCommentUrl(source.getCommentUrl());
        target.setNotes(toInternationalString(source.getNotes()));
        target.setNotesUrl(source.getNotesUrl());

        return target;
    }

    // TODO pasar a librería común
    @Override
    public org.siemac.metamac.rest.common.v1_0.domain.Error toError(Exception exception) {
        org.siemac.metamac.rest.common.v1_0.domain.Error error = new org.siemac.metamac.rest.common.v1_0.domain.Error();
        error.getErrorItems().addAll(toErrorItems(exception));
        return error;
    }

    private List<ErrorItem> toErrorItems(Exception exception) {

        List<ErrorItem> errorItems = new ArrayList<ErrorItem>();
        if (exception instanceof MetamacException) {
            MetamacException metamacException = (MetamacException) exception;
            for (MetamacExceptionItem metamacExceptionItem : metamacException.getExceptionItems()) {
                ErrorItem errorItem = new ErrorItem();
                errorItem.setCode(metamacExceptionItem.getCode());
                errorItem.setMessage(metamacExceptionItem.getMessage());
                if (metamacExceptionItem.getMessageParameters() != null) {
                    for (int i = 0; i < metamacExceptionItem.getMessageParameters().length; i++) {
                        Serializable messageParameter = metamacExceptionItem.getMessageParameters()[i];
                        String parameter = null;
                        if (messageParameter instanceof String) {
                            parameter = messageParameter.toString();
                        } else if (messageParameter instanceof String[]) {
                            parameter = Arrays.deepToString((String[]) messageParameter);
                        } else {
                            parameter = messageParameter.toString();
                        }
                        errorItem.getParameters().add(parameter);
                    }
                }
                errorItems.add(errorItem);
            }
        } else {
            ErrorItem errorItem = new ErrorItem();
            errorItem.setCode(CommonServiceExceptionType.UNKNOWN.getCode());
            errorItem.setMessage(exception.getMessage());
            errorItems.add(errorItem);
        }
        return errorItems;
    }

    private List<Resource> familiesToResources(Set<Family> sources, String apiUrl) {
        List<Resource> targets = new ArrayList<Resource>();
        if (sources == null) {
            return targets;
        }
        for (Family source : sources) {
            Resource target = familyToResource(source, apiUrl);
            targets.add(target);
        }
        return targets;
    }

    private Resource familyToResource(Family source, String apiUrl) {
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

    private List<Resource> instancesToResources(List<Instance> sources, String apiUrl) {
        List<Resource> targets = new ArrayList<Resource>();
        if (sources == null) {
            return targets;
        }
        for (Instance source : sources) {
            Resource target = instanceToResource(source, apiUrl);
            targets.add(target);
        }
        return targets;
    }

    private Resource instanceToResource(Instance source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getCode());
        target.setKind(RestInternalConstants.KIND_INSTANCE);
        target.setLink(toInstanceLink(source, apiUrl));
        target.setTitle(toInternationalString(source.getTitle()));
        return target;
    }

    private List<Resource> externalItemsToResources(Set<ExternalItem> sources) {
        if (sources == null) {
            return null;
        }
        List<Resource> targets = new ArrayList<Resource>();
        for (ExternalItem source : sources) {
            Resource target = externalItemToResource(source.getExt());
            targets.add(target);
        }
        return targets;
    }

    private Resource externalItemToResource(ExternalItemBt source) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getCodeId());
        target.setKind(source.getType().name());
        target.setLink(toExternalItemLink(source));
        target.setTitle(null); // TODO pendiente de que se añada title en ExternalItem
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

    private Link toInstanceLink(org.siemac.metamac.statistical.operations.core.domain.Instance instance, String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHref(apiUrl, RestInternalConstants.LINK_SUBPATH_OPERATIONS, instance.getOperation().getCode(), RestInternalConstants.LINK_SUBPATH_INSTANCES, instance.getCode()));
        return link;
    }

    private Link toExternalItemLink(ExternalItemBt externalItemBt) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(externalItemBt.getUriInt());
        return link;
    }

    private String createLinkHref(String apiUrl, String... subpaths) {
        UriComponentsBuilder uriComponentsBuilder = UriComponentsBuilder.fromHttpUrl(apiUrl);
        if (subpaths != null) {
            for (int i = 0; i < subpaths.length; i++) {
                uriComponentsBuilder = uriComponentsBuilder.pathSegment(subpaths[i]);
            }
        }
        return uriComponentsBuilder.build().toUriString();
    }
}
