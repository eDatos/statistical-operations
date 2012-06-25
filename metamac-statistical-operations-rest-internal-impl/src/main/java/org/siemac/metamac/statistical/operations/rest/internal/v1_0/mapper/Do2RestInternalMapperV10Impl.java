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
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.Link;
import org.siemac.metamac.rest.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.rest.common.v1_0.domain.ErrorItem;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;

@Component
public class Do2RestInternalMapperV10Impl implements Do2RestInternalMapperV10 {

    // @Context
    // private MessageContext context; // Always null in this bean (not in Service)...

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
        target.setSurveyType(surveyTypeToResource(source.getSurveyType(), apiUrl));
        target.setOfficialityType(officialityTypeToResource(source.getOfficialityType(), apiUrl));
        target.setIndicatorSystem(source.getIndicatorSystem());
        target.getProducers().addAll(externalItemsToResources(source.getProducer()));
        target.getRegionalResponsibles().addAll(externalItemsToResources(source.getRegionalResponsible()));
        target.getRegionalContributors().addAll(externalItemsToResources(source.getRegionalContributor()));
        target.setInternalInventoryDate(toDate(source.getInternalInventoryDate()));
        target.setCurrentlyActive(source.getCurrentlyActive());
        target.setStatus(source.getStatus() != null ? source.getStatus().name() : null);
        target.setProcStatus(source.getProcStatus().name());
        target.getPublishers().addAll(externalItemsToResources(source.getPublisher()));
        target.setRelPolUsAc(toInternationalString(source.getRelPolUsAc()));
        target.setRelPolUsAcUrl(source.getRelPolUsAcUrl());
        target.setReleaseCalendar(source.getReleaseCalendar());
        target.setReleaseCalendarAccess(source.getReleaseCalendarAccess());
        target.getUpdateFrequencies().addAll(externalItemsToResources(source.getUpdateFrequency()));
        target.setCurrentInstance(instanceToResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_EXTERNALLY), apiUrl));
        target.setCurrentInternalInstance(instanceToResource(getInstanceInProcStatus(source.getInstances(), ProcStatusEnum.PUBLISH_INTERNALLY), apiUrl));
        target.setInventoryDate(toDate(source.getInventoryDate()));
        target.setRevPolicy(toInternationalString(source.getRevPolicy()));
        target.setRevPolicyUrl(source.getRevPolicyUrl());
        target.setRevPractice(toInternationalString(source.getRevPractice()));
        target.setRevPracticeUrl(source.getRevPracticeUrl());
        // TODO LEGAL_ACTS, DATA_SHARING, CONFIDENTIALITY_POLICY, CONFIDENTIALITY_DATA_TREATMENT. No están en OperationBase
        target.setComment(toInternationalString(source.getComment()));
        target.setCommentUrl(source.getCommentUrl());
        target.setNotes(toInternationalString(source.getNotes()));
        target.setNotesUrl(source.getNotesUrl());
        target.setParent(toOperationParent(apiUrl));
        target.getchildren().addAll(toOperationChildren(source, apiUrl));
        return target;
    }
    // TODO pasar a librería común toError
    @Override
    public Error toError(Exception exception) {
        Error error = new Error();
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

    private Resource surveyTypeToResource(SurveyType source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
        return target;
    }

    private Resource officialityTypeToResource(OfficialityType source, String apiUrl) {
        if (source == null) {
            return null;
        }
        Resource target = new Resource();
        target.setId(source.getIdentifier());
        target.setTitle(toInternationalString(source.getDescription()));
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
        target.setId(source.getCodeId()); // TODO próximamente se cambiará por urn en ExternalItem
        target.setKind(source.getType().name());
        target.setLink(toExternalItemLink(source));
        target.setTitle(null); // TODO se añadirá Title a ExternalItem
        return target;
    }

    private Resource toOperationParent(String apiUrl) {
        Resource target = new Resource();
        target.setKind(RestInternalConstants.KIND_OPERATIONS);
        target.setLink(toOperationsLink(apiUrl));
        return target;
    }

    private List<Resource> toOperationChildren(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        List<Resource> targets = new ArrayList<Resource>();
        
        // Instances
        Resource instancesTarget = new Resource();
        instancesTarget.setKind(RestInternalConstants.KIND_INSTANCES);
        instancesTarget.setLink(toInstancesLink(operation, apiUrl));
        targets.add(instancesTarget);
        
        return targets;
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
        link.setHref(createLinkHrefOperation(apiUrl, operation));
        return link;
    }

    private Link toOperationsLink(String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHrefOperations(apiUrl));
        return link;
    }

    private Link toFamilyLink(org.siemac.metamac.statistical.operations.core.domain.Family family, String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHrefFamily(apiUrl, family));
        return link;
    }

    private Link toInstancesLink(org.siemac.metamac.statistical.operations.core.domain.Operation operation, String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHrefInstances(apiUrl, operation));
        return link;
    }

    private Link toInstanceLink(org.siemac.metamac.statistical.operations.core.domain.Instance instance, String apiUrl) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(createLinkHrefInstance(apiUrl, instance));
        return link;
    }

    private Link toExternalItemLink(ExternalItemBt externalItemBt) {
        Link link = new Link();
        link.setRel(RestInternalConstants.LINK_SELF);
        link.setHref(externalItemBt.getUriInt());
        return link;
    }

    private String createLinkHrefOperations(String apiUrl) {
        return createLinkHref(apiUrl, RestInternalConstants.LINK_SUBPATH_OPERATIONS);
    }

    private String createLinkHrefOperation(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkOperations = createLinkHrefOperations(apiUrl);
        return createLinkHref(linkOperations, operation.getCode());
    }

    private String createLinkHrefInstances(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Operation operation) {
        String linkOperation = createLinkHrefOperation(apiUrl, operation);
        return createLinkHref(linkOperation, RestInternalConstants.LINK_SUBPATH_INSTANCES);
    }

    private String createLinkHrefInstance(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Instance instance) {
        String linkOperation = createLinkHrefInstances(apiUrl, instance.getOperation());
        return createLinkHref(linkOperation, instance.getCode());
    }

    private String createLinkHrefFamilies(String apiUrl) {
        return createLinkHref(apiUrl, RestInternalConstants.LINK_SUBPATH_FAMILIES);
    }

    private String createLinkHrefFamily(String apiUrl, org.siemac.metamac.statistical.operations.core.domain.Family family) {
        String linkFamilies = createLinkHrefFamilies(apiUrl);
        return createLinkHref(linkFamilies, family.getCode());
    }

    private String createLinkHref(String baseLink, String additionalPath) {
        UriComponentsBuilder uriComponentsBuilder = UriComponentsBuilder.fromHttpUrl(baseLink);
        uriComponentsBuilder = uriComponentsBuilder.pathSegment(additionalPath);
        return uriComponentsBuilder.build().toUriString();
    }

    private Instance getInstanceInProcStatus(List<Instance> instances, ProcStatusEnum procStatus) {

        for (Instance instance : instances) {
            if (procStatus.equals(instance.getProcStatus())) {
                return instance;
            }
        }
        return null;
    }
}
